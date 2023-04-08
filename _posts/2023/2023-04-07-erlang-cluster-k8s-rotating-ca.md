---
title: "Erlang cluster on Kubernetes: Rotating CA certificates"
short_title: "Rotating CA certificates"
date: 2023-04-07T10:31:00.000Z
tags: erlang kubernetes certificates
---

About 3 months ago, the Erlang nodes in my [cluster]({% post_url 2022/2022-12-21-erlang-cluster-k8s-intro %}) stopped
talking to each other. This was caused by the [CA certificate expiring]({% post_url
2023/2023-01-07-erlang-cluster-k8s-certificate-expired %}). In that post, I asked "How _do_ we roll the CA certificate
without downtime?". This post explores some options.

The root cause of the failure is that I set the CA certificate expiry to 30 days (the `openssl req` default), and that
when the pods restarted (because of a K3s node upgrade) the Erlang nodes no longer trusted each other.

I solved it, short term, by reissuing the CA certificate and restarting the pods. I could have deferred the problem for
even longer by issuing, say, a 3 year CA certificate, but all that does is push the problem down the road.

The _correct_ solution is to rotate the CA certificate deliberately on a schedule. For example, you might issue a 6
month CA certificate, and generate a fresh one every 5 months. The question then becomes, as above: "How _do_ we roll
the CA certificate without downtime?".

To figure that out, we need to look at how the CA certificate is used when one Erlang node connects to another:

- If both nodes are using certificates issued by the old CA (and it's not yet expired), we're good.
- Similarly, if both nodes are using certificates issued by the new CA, we're good.
- If an "old" node connects to a "new" node then the "old" node must trust the new CA, _and_ (because we're using mutual
  TLS), the "new" node must trust the old CA.
- Similarly, if a "new" node connects to an "old" node, the same logic applies.

This means that the `/certs/ca.crt` file should contain _all_ of the CA certificates that we want to trust, both old and
new.

There are a few questions that need addressing first:

1. How do we update the `/certs/ca.crt` file on an old node without restarting the pod?
2. Does Erlang reload the file when it changes? If not, how do we force it to?

## Updating K8s secrets

You can expose Kubernetes Secrets to a pod via environment variables, or by mounting them as volumes.

You can't update the environment variables without restarting the pod (because they're copied into the process memory at
startup).

You _can_ update the mounted secrets. After a short delay, the files in the mounted volume will be updated to contain
the new secrets.

## Creating the trusted CA secret

We originally generated the CA certificate like this:

```sh
openssl req -new -x509 -key erlclu-ca.key -sha256 \
    -subj "/C=GB/L=London/O=differentpla.net/CN=erlclu CA" -out erlclu-ca.crt
```

Or we can retrieve it from the existing keypair secret as follows:

```sh
kubectl --namespace erlclu get secret erlclu-ca-key-pair -o json | \
    jq -r '.data."tls.crt"' | \
    base64 -d > erlclu-ca.crt
```

We can put that in a secret as follows:

```sh
kubectl --namespace erlclu create secret generic erlclu-ca-certificates \
    --from-file=ca.crt=erlclu-ca.crt
```

(I chose the name "erlclu-ca-certificates" to make it clear that it's not the normal list of trusted CA certificates)

It's at this point that I need to change the way that the `/certs/ca.crt` file is created. At the moment, the [init
container]({% post_url 2022/2022-12-22-erlang-cluster-k8s-certificate-requests-cert-manager %}) pulls it from the signed
certificate request:

```sh
echo "$res" | jq -r '.status.ca' | base64 -d > "$CERTS_DIR/ca.crt"
```

But we want that file to contain multiple trusted CA certificates, which means we need to stop doing this, and to use
the `erlclu-ca-certificates` secret instead. This means moving stuff around: instead of a single volume `/certs`,
containing each node's key (and certificate) and the CA certificate, we'll need two volumes.

I opted for `/certs/my` for the node's key (and certificate) and `/certs/ca` for the `erlclu-ca-certificates` secret.
See the [0.10.4 tag](https://github.com/rlipscombe/erlang-cluster/tree/0.10.4) on Github for details.

## Updating the trusted CA secret

We can issue a new CA certificate as follows:

```sh
cert_timestamp="$(date +%FT%H-%M-%S)"
openssl req -new -x509 -key erlclu-ca.key -sha256 \
    -subj "/C=GB/L=London/O=differentpla.net/CN=erlclu CA $cert_timestamp" -out "erlclu-ca-$cert_timestamp.crt"
```

Then we update the secret:

```sh
cat erlclu-ca-*.crt > ca-certificates.crt
kubectl --namespace erlclu delete secret erlclu-ca-certificates
kubectl --namespace erlclu create secret generic erlclu-ca-certificates \
    --from-file=ca.crt=ca-certificates.crt
```

And we can confirm that the `/certs/ca/ca.crt` file is updated without restarting the pod:

```
(erlclu@10.42.5.149)1> {ok, CACerts} = file:read_file("/certs/ca/ca.crt").
{ok,<<"-----BEGIN CERTIFICATE-----\nMIIB7zCCAZWgAwIBAgIUNnhsOH5fiZVFC3B+oQ7wTgKTMwgwCgYIKoZIzj0EAwIw\nTTELMAkGA1UEBhM"...>>}
(erlclu@10.42.5.149)2> length(public_key:pem_decode(CACerts)).
2
```

Where previously there was only one certificate in the file, there are now two.

## Rotating the CA certificate

Optionally create a new key as follows:

```sh
cert_timestamp="$(date +%FT%H-%M-%S)"
openssl ecparam -name prime256v1 -genkey -noout -out erlclu-ca-$cert_timestamp.key
```

Create a new certificate as follows:

```sh
cert_timestamp="$(date +%FT%H-%M-%S)"
openssl req -new -x509 -key erlclu-ca-$cert_timestamp.key -sha256 \
    -subj "/C=GB/L=London/O=differentpla.net/CN=erlclu CA $cert_timestamp" -out "erlclu-ca-$cert_timestamp.crt"
```

If we update the _cert-manager_ keypair at this point, new or restarted pods will fail to join the cluster, because the
existing nodes don't trust the newly-issued certificates. So we have to update the list of trusted certificates first:

```sh
kubectl --namespace erlclu get secret erlclu-ca-certificates -o json | \
    jq -r '.data."ca.crt"' | base64 -d > erlclu-ca-existing.crt

./filter-ca-certs.escript <(cat erlclu-ca-*.crt) > ca-certificates.crt

kubectl --namespace erlclu delete secret erlclu-ca-certificates
kubectl --namespace erlclu create secret generic erlclu-ca-certificates \
    --from-file=ca.crt=ca-certificates.crt
```

The `filter-ca-certs.escript` file is [here](https://github.com/rlipscombe/erlang-cluster/blob/main/certs/filter-ca-certs.escript).

Now we can update the _cert-manager_ keypair as follows:

```sh
kubectl --namespace erlclu delete secret erlclu-ca-key-pair
kubectl --namespace erlclu create secret tls erlclu-ca-key-pair \
    --cert=erlclu-ca-$cert_timestamp.crt \
    --key=erlclu-ca-$cert_timestamp.key
```

Note that it takes a short while for the updated `erlclu-ca-certificates` secret to be deployed. If you scale up the
deployment before this is complete, you'll get two distinct clusters. This is temporary; it resolves itself after a few
minutes.
