---
title: "Erlang cluster on Kubernetes: Using a namespace-scoped issuer"
short_title: "Namespace-scoped issuer"
date: 2022-12-23T17:06:00.000Z
layout: series
series: erlang-cluster-k8s
tags: kubernetes cert-manager
---

In [an earlier post]({% post_url 2022/2022-12-22-erlang-cluster-k8s-certificate-requests-cert-manager %}), I used a
`ClusterIssuer` that I originally created when [first setting up cert-manager]({% post_url 2022/2022-02-06-cert-manager
%}). That needs fixing.

## Create the CA Keypair

Note that this is just enough to get it working; I've not considered expiry, usages, whatever. Depending on your
security policies, you might want to keep a root CA in an HSM and use an intermediate CA.

```bash
openssl ecparam -name prime256v1 -genkey -noout -out erlclu-ca.key
openssl req -new -x509 -key erlclu-ca.key -sha256 \
    -subj "/C=GB/L=London/O=differentpla.net/CN=erlclu CA" -out erlclu-ca.crt
```

## CA Secret

_cert-manager_ will look for the CA certificate and key in a K8s secret:

```bash
kubectl --namespace erlclu create secret tls erlclu-ca-key-pair \
    --cert=erlclu-ca.crt \
    --key=erlclu-ca.key
```

## Create an Issuer object

For a namespace-scoped CA, we need an `Issuer` object:

```yaml
apiVersion: cert-manager.io/v1
kind: Issuer
metadata:
  namespace: erlclu
  name: erlclu-issuer
spec:
  ca:
    secretName: erlclu-ca-key-pair
```

## Fix the deployment

[Previously]({% post_url 2022/2022-12-22-erlang-cluster-k8s-certificate-requests-cert-manager %}), I used a
`ClusterIssuer` object, passed to the init container with thhe `ISSUER_KIND` and `ISSUER_NAME` environment variables. To
use the new `Issuer`, we need to change those:

```yaml
env:
  #...
  - name: ISSUER_KIND
    value: Issuer
  - name: ISSUER_NAME
    value: erlclu-issuer
```

Then we just redeploy the pods and watch as they request (and get) certificates from the correct issuer:

```
NAME                      APPROVED   DENIED   READY   ISSUER                  REQUESTOR                                         AGE
erlclu-54b96fdb7c-zqlh8   True                True    erlclu-issuer           system:serviceaccount:erlclu:erlclu               1s
erlclu-54b96fdb7c-sdcts   True                True    erlclu-issuer           system:serviceaccount:erlclu:erlclu               1s
...
```
