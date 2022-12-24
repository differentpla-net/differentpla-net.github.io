---
title: "Erlang cluster on Kubernetes: Using the generated certificates"
short_title: "Using the generated certificates"
date: 2022-12-22T10:04:00.000Z
layout: series
series: erlang-cluster-k8s
tags: erlang kubernetes
---

In the previous two posts, we [generated signing requests with OpenSSL]({% post_url
2022/2022-12-22-erlang-cluster-k8s-certificate-requests-openssl %}) and [submitted them to _cert-manager_]({% post_url
2022/2022-12-22-erlang-cluster-k8s-certificate-requests-cert-manager %}). In this post, we'll actually _use_ the
generated certificates for mutual TLS.

## Mounting the /certs volume

The init container generated some certificates and wrote them to the `/certs` volume. We need to mount that volume into the main container:

```yaml
volumeMounts:
  - name: erlclu-dist-tls
    mountPath: /secrets
  - name: tls-dist
    mountPath: /certs
```

I ought to get rid of the `/secrets` mount as well -- but [Many More Much Smaller
Steps](https://www.geepawhill.org/2021/09/29/many-more-much-smaller-steps-first-sketch/) -- so I'll do that later.

## Use the certificates for the server

While doing small steps, I'll make the initial change (in `inet_tls_dist.config`) to only have the server use our new
certificates:

```erlang
[
    {server, [
        {certfile, "/certs/tls-dist.crt"},
        {keyfile, "/certs/tls-dist.key"},
        %...
```

Tiny steps really helps at this point, because TLS (and particularly mutual TLS) is fiddly. It's a lot easier if you can
identify precisely _what you just changed_ that broke everything.

## Server verification

Since that works, we can move on to the next step. The client should verify the server:

```erlang
    %...
    {client, [
        {verify, verify_peer},
        {cacertfile, "/certs/ca.crt"},
    %...
```

## Require client certs

The server should require client certificates, and the client should offer them:

```erlang
[
    {server, [
        %...
        {verify, verify_peer},
        {fail_if_no_peer_cert, true},
        {cacertfile, "/certs/ca.crt"},
        %...
    ]},
    {client, [
        {certfile, "/certs/tls-dist.crt"},
        {keyfile, "/certs/tls-dist.key"},
        {verify, verify_peer},
        {cacertfile, "/certs/ca.crt"},
        {secure_renegotiate, true}
    ]}
].
```

Remember: peer verification requires adding `certfile` and `keyfile` to the peer that provides the certificate, and
`{verify, verify_peer}` and `cacertfile` to the peer that verifies the certificate.

For the server, we also have to add `{fail_if_no_peer_cert, true}` to require client certificates.

Note: In reality, I actually did this as two tiny steps, accepting that it would fail after the first one.

## Stop using secrets for certs

Now that the self-provisioned certificates are working, we can stop using the secret that [we created previously]({% post_url 2022/2022-12-22-erlang-cluster-k8s-tls-distribution %}#kubernetes-tls-secret).

This means deleting the `erlclu-dist-tls` volumes from the deployment, and then deleting the secret:

```
kubectl --namespace erlclu delete secret erlclu-dist-tls
```

## Limitations

Because we use an init container to generate the certificates, they will not be renewed when they expire. This can be
mitigated by simply restarting the pods before this happens, or by adding a sidecar container (based on the init
container) to renew the certificates periodically.
