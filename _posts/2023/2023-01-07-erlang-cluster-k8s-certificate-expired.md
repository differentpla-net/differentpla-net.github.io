---
title: "Erlang cluster on Kubernetes: Fatal - Certificate Expired"
short_title: "Fatal - Certificate Expired"
date: 2023-01-07T17:11:00.000Z
tags: erlang kubernetes
---

The nodes in the [cluster]({% post_url 2022/2022-12-21-erlang-cluster-k8s-intro %}) stopped talking to each other at some point. I only noticed this afternoon after investigating [some other problem]({% post_url 2023/2023-01-07-network-connectivity-incident-review %}).

Looking in the logs, I see the following:

```
% kubectl --namespace erlclu logs erlclu-54b96fdb7c-tdtcl
2023-01-07T14:50:58.126136+00:00 info: Supervisor: {<0.21079.3>,tls_dyn_connection_sup}. Started: id=sender,pid=<0.21080.3>.
2023-01-07T14:50:58.126674+00:00 info: Supervisor: {<0.21079.3>,tls_dyn_connection_sup}. Started: id=receiver,pid=<0.21081.3>.
2023-01-07T14:50:58.228820+00:00 notice: TLS server: In state certify received CLIENT ALERT: Fatal - Certificate Expired
2
```

That's a bit of a surprise; the pods were restarted this morning during an [Ubuntu upgrade]({% post_url
2023/2023-01-07-k3s-apt-upgrade %}), so they should have been provisioned with brand-new certificates.

So, I guess the question needs answering: which certificate has expired, and where did I screw that up?

## Things to investigate

I'll draw up an initial list of things to investigate:

1. Do the nodes and pods have the correct time?
2. What's the expiry on the [root CA]({% post_url 2022/2022-12-23-erlang-cluster-k8s-namespace-issuer %})?
3. What are the expiries on the mTLS certificates?

## CA certificate

The easiest thing to check is the CA certificate, so let's do that:

```
% kubectl --namespace erlclu get secret erlclu-ca-key-pair -o json | \
    jq -r '.data."tls.crt"' | \
    base64 -d > erlclu-ca.crt
% openssl x509 -in erlclu-ca.crt -text -noout
Certificate:
    ...
        Validity
            Not Before: Dec  2 08:25:09 2022 GMT
            Not After : Jan  1 08:25:09 2023 GMT
    ...
```

Well; there's your problem. Apparently the certificate was issued with a 30d expiry and expired 6 days ago.

## Node certificate

How to get the validity of the node certificates?

```
% kubectl --namespace erlclu exec -it deployments/erlclu -- /bin/sh
Defaulted container "erlclu" out of: erlclu, erlclu-init (init)
/ $ cd /certs
/certs $ openssl x509 -in tls-dist.crt -text -noout
Certificate:
    ...
        Validity
            Not Before: Jan  7 09:44:27 2023 GMT
            Not After : Apr  7 09:44:27 2023 GMT
    ...
```

Three months, apparently. That [seems to be](https://cert-manager.io/v1.2-docs/faq/#if-renewbefore-or-duration-is-not-defined-what-will-be-the-default-value) the default for _cert-manager_ if the request doesn't specify `duration` or `renewBefore`.

I can live with that.

### Do the pods have the correct time?

I suspect that various K8s weirdness would have occurred if not, but here's how to check anyway:

```
% kubectl --namespace erlclu \
    get pods -l app=erlclu \
        -o jsonpath='{range .items[*]}{.metadata.name}{"\n"}{end}' | \
    xargs -I {} \
        kubectl --namespace erlclu exec {} -c erlclu -- date

Sat Jan  7 17:49:50 UTC 2023
Sat Jan  7 17:49:51 UTC 2023
Sat Jan  7 17:49:51 UTC 2023
Sat Jan  7 17:49:51 UTC 2023
Sat Jan  7 17:49:51 UTC 2023
```

Given that -- at the time of writing -- it's `Sat Jan  7 17:49:51 UTC 2023`, that seems OK.

## Immediate Actions

But not until we've completed the investigation; this isn't a business-critical system.

### Reissue the CA certificate

At this time, I'm _not_ going to issue a longer-lived CA certificate, because I _want_ it to break. This will remind me to
find some time to look at "Further Actions" below.

```sh
# Create a new keypair and certificate
openssl ecparam -name prime256v1 -genkey -noout -out erlclu-ca.key
openssl req -new -x509 -key erlclu-ca.key -sha256 \
    -subj "/C=GB/L=London/O=differentpla.net/CN=erlclu CA" -out erlclu-ca.crt

# Recreate the K8s secret
kubectl --namespace erlclu delete secret erlclu-ca-key-pair

kubectl --namespace erlclu create secret tls erlclu-ca-key-pair \
    --cert=erlclu-ca.crt \
    --key=erlclu-ca.key

# Nuke the pods and redeploy everything
kubectl --namespace erlclu delete deployment erlclu
kubectl --namespace erlclu apply -f k8s/deployment.yaml
```

## Further Actions

- Did clustering break 6 days ago and I just didn't notice?
  - Investigate metrics for cluster membership.
- Or did it keep going until I restarted the pods this morning?
  - Investigate and characterise what Erlang distribution does when the CA expires.
  - Ditto, but for node certificate expiration.
- Can we alert from the init container if the CA certificate has expired? Or just fail to start the pod, and then alert
  on that?
- Can we fail pod startup if _our_ certificate (or the CA) has expired?
  - Does that suggest a custom distribution module?
- What _are_ our options for CA expiry? How _do_ we roll the CA certificate without downtime?
