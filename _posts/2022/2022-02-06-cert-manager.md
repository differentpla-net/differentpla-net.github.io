---
title: "Installing cert-manager"
date: 2022-02-06T15:18:00Z
tags: kubernetes cert-manager
layout: series
series: k3s
---

Up to this point, I've been creating and installing certificates manually. Let's see if
[cert-manager](https://cert-manager.io/) will make that easier.

## Installation

```
$ kubectl apply -f https://github.com/cert-manager/cert-manager/releases/download/v1.7.1/cert-manager.yaml
```

```
$ kubectl --namespace cert-manager get all
NAME                                         READY   STATUS    RESTARTS   AGE
pod/cert-manager-6d8d6b5dbb-qfxr5            1/1     Running   0          7m4s
pod/cert-manager-webhook-85fb68c79b-gtj2z    1/1     Running   0          7m4s
pod/cert-manager-cainjector-d6cbc4d9-tw5pl   1/1     Running   0          7m4s

NAME                           TYPE        CLUSTER-IP      EXTERNAL-IP   PORT(S)    AGE
service/cert-manager           ClusterIP   10.43.43.27     <none>        9402/TCP   7m5s
service/cert-manager-webhook   ClusterIP   10.43.181.148   <none>        443/TCP    7m5s

NAME                                      READY   UP-TO-DATE   AVAILABLE   AGE
deployment.apps/cert-manager              1/1     1            1           7m5s
deployment.apps/cert-manager-webhook      1/1     1            1           7m5s
deployment.apps/cert-manager-cainjector   1/1     1            1           7m5s

NAME                                               DESIRED   CURRENT   READY   AGE
replicaset.apps/cert-manager-6d8d6b5dbb            1         1         1       7m5s
replicaset.apps/cert-manager-webhook-85fb68c79b    1         1         1       7m5s
replicaset.apps/cert-manager-cainjector-d6cbc4d9   1         1         1       7m5s
```

## Certificate Issuer

Before we can issue any certificates, we need to create an `Issuer` or `ClusterIssuer` resource. These represent a
certificate authority. The former issues certificates for the namespace that it's installed in. The latter can issue
certificates for all namespaces.

Because my cluster isn't multi-tenant, and everything uses the same root CA, I'm going to create a `ClusterIssuer`.
Because I'm using my own root CA (rather than, say, Let's Encrypt), I'll use the `CA` type.

### k3s-ca-cluster-issuer.yaml

```yaml
apiVersion: cert-manager.io/v1
kind: ClusterIssuer
metadata:
  name: k3s-ca-cluster-issuer
spec:
  ca:
    secretName: k3s-ca-key-pair
```

```
kubectl apply -f k3s-ca-cluster-issuer.yaml
```

If you take a look at the logs for the `cert-manager` pod, you'll see that it's detected the `ClusterIssuer`, but can't
find the secret (wrapped for readability):

```
E0206 15:29:47.347247       1 setup.go:48] cert-manager/clusterissuers/setup
    "msg"="error getting signing CA TLS certificate"
    "error"="secret \"k3s-ca-key-pair\" not found"
    "resource_kind"="ClusterIssuer"
    "resource_name"="k3s-ca-cluster-issuer"
    "resource_namespace"=""
    "resource_version"="v1"
```

By [default](https://cert-manager.io/docs/faq/cluster-resource/), `ClusterIssuer` resources look for the named secret in
the `cert-manager` namespace, so:

```bash
kubectl --namespace cert-manager \
    create secret tls k3s-ca-key-pair \
        --cert=k3s-ca.crt \
        --key=k3s-ca.key
```

## Issuing a certificate

To request a certificate, create a `Certificate` resource. The
[documentation](https://cert-manager.io/docs/usage/certificate/) is exhaustive, but a minimal example would look like this:

```yaml
apiVersion: cert-manager.io/v1
kind: Certificate
metadata:
  name: example-k3s-differentpla-net
  namespace: default
spec:
  secretName: example-k3s-differentpla-net-tls
  issuerRef:
    name: k3s-ca-cluster-issuer
    kind: ClusterIssuer
  dnsNames:
  - example.k3s.differentpla.net
```

If we apply the manifest:

```
$ kubectl apply -f example-k3s-differentpla-net-certificate.yaml
certificate.cert-manager.io/example-k3s-differentpla-net created

$ kubectl --namespace default get secret
NAME                                        TYPE                                  DATA   AGE
example-k3s-differentpla-net-tls            kubernetes.io/tls                     3      40s
```

```
$ kubectl --namespace default get secret example-k3s-differentpla-net-tls -o yaml
apiVersion: v1
data:
  ca.crt: LS0tLS1...
  tls.crt: LS0tLS1...
  tls.key: LS0tLS1...
kind: Secret
metadata:
  annotations:
    cert-manager.io/alt-names: example.k3s.differentpla.net
    cert-manager.io/certificate-name: example-k3s-differentpla-net
    cert-manager.io/common-name: ""
    cert-manager.io/ip-sans: ""
    cert-manager.io/issuer-group: ""
    cert-manager.io/issuer-kind: ClusterIssuer
    cert-manager.io/issuer-name: k3s-ca-cluster-issuer
    cert-manager.io/uri-sans: ""
  creationTimestamp: "2022-02-06T15:45:23Z"
  name: example-k3s-differentpla-net-tls
  namespace: default
  resourceVersion: "5999398"
  uid: 7f59a415-1b7c-40f3-9027-0453df5ee4e4
type: kubernetes.io/tls
```

And then, of course, we can reference that secret, either directly in an Ingress (of which more later), an IngressRoute,
or in a mount or environment variable.

## Inspecting the key/certificate

Let's take a look at the actual certificate and key, to see what's in them.

```bash
{% raw %}kubectl --namespace default get secret example-k3s-differentpla-net-tls \
    --template="{{index .data \"tls.key\" | base64decode}}" > example-k3s-differentpla-net.key
kubectl --namespace default get secret example-k3s-differentpla-net-tls \
    --template="{{index .data \"tls.crt\" | base64decode}}" > example-k3s-differentpla-net.crt{% endraw %}
```

```
Certificate:
    Data:
        Version: 3 (0x2)
        Serial Number:
            ea:80:...
        Signature Algorithm: ecdsa-with-SHA256
        Issuer: C = GB, L = London, O = differentpla.net, CN = differentpla.net k3s CA
        Validity
            Not Before: Feb  6 15:45:23 2022 GMT
            Not After : May  7 15:45:23 2022 GMT
        Subject:
        Subject Public Key Info:
            Public Key Algorithm: rsaEncryption
                RSA Public-Key: (2048 bit)
                Modulus:
                    00:d4:...
                Exponent: 65537 (0x10001)
        X509v3 extensions:
            X509v3 Key Usage: critical
                Digital Signature, Key Encipherment
            X509v3 Basic Constraints: critical
                CA:FALSE
            X509v3 Authority Key Identifier:
                keyid:C6:5D:...

            X509v3 Subject Alternative Name: critical
                DNS:example.k3s.differentpla.net
    Signature Algorithm: ecdsa-with-SHA256
         30:45:...
```

Comparing that with the the certificates created by my `certs` script, I can see a couple of issues:

- It used an RSA key, rather than my preferred EC key.
- The Extended Key Usage extension is missing.
- There's no subject. Using `commonName` is deprecated in favour of the `subjectAlternativeName` extension, but it just
  seems weird.

Some tweaks to the `Certificate` manifest can fix all of that:

```yaml
...
  commonName: example.k3s.differentpla.net
  dnsNames:
  - example.k3s.differentpla.net
  privateKey:
    algorithm: ECDSA
  usages:
    - server auth
    - client auth
```

To recreate the certificate, delete the secret. cert-manager will recreate it. Alternatively, wait for it to expire
(default 3 months).

## Ingress

If you're using Ingress resources, you don't need to create Certificate resources yourself. cert-manager will
automatically issue certificates for them, provided you've [annotated the Ingress appropriately](https://cert-manager.io/docs/usage/ingress/).

However, it _doesn't_ seem that you can change the private key algorithm by using annotations.
See [cert-manager#4769](https://github.com/cert-manager/cert-manager/issues/4769).

## IngressRoute

Per [the documentation](https://doc.traefik.io/traefik/v2.0/providers/kubernetes-crd/):

> When using the Traefik Kubernetes CRD Provider, unfortunately Cert-Manager cannot interface directly with the CRDs
> yet, but this is being worked on by our team.

The workaround seems to be to create a fake Ingress along with the real IngressRoute. I've not tried that yet.
