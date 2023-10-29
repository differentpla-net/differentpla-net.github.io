---
title: "K3s: You must be logged in to the server (Unauthorized)"
date: 2023-10-29T13:53:00.000Z
tags: k3s kubernetes yq
---

This afternoon, I couldn't run `kubectl get namespaces` against my K3s cluster. Instead, I got an `Unauthorized` error.

```
error: You must be logged in to the server (Unauthorized)
```

When I ran the same command on the server node, I also got the following:

```
E1029 13:47:01.971564 2308067 memcache.go:265] couldn't get current server API group list: the server has asked for the client to provide credentials
```

I upgraded K3s recently, but I'm pretty sure it was working after I did that, and it's never been broken before. K3s
uses client certificates for authentication, and I suspect that the certificate has expired or something.

Comparing the `/etc/rancher/k3s/k3s.yaml` file with `~/.kube/config` (in `$KUBECONFIG`), there are some differences.
Let's dig in:

```sh
$ yq '.users[] | select(.name == "default") | .user.client-certificate-data' < $KUBECONFIG
LS0t...tCg==
```

That's a base64-encoded ... something (snipped). Is it a certificate?

```sh
$ yq '.users[] | select(.name == "default") | .user.client-certificate-data' < $KUBECONFIG | base64 -d
-----BEGIN CERTIFICATE-----
MII...
-----END CERTIFICATE-----
-----BEGIN CERTIFICATE-----
MII...
-----END CERTIFICATE-----
```

It's two certificates. Presumably one of them is the client certificate and one of them is the issuing CA. The `openssl x509` command will only display the first certificate, so we need to split the certificates into separate files:

```sh
$ yq '.users[] | select(.name == "default") | .user.client-certificate-data' < $KUBECONFIG | base64 -d | \
    awk 'BEGIN {n=1} x == 1 {n++; x=0} /END CERTIFICATE/ {x=1} {print > "cert" n ".pem"}'
```

Then we can take a look:

```
$ openssl x509 -text -noout < cert1.pem
Certificate:
    Data:
        ...
        Validity
            Not Before: Oct 29 11:24:58 2022 GMT
            Not After : Oct 29 11:24:58 2023 GMT
        Subject: O = system:masters, CN = system:admin
        ...
```

Yeah, that expired. _This morning_.

For completeness, the second cert is the CA:

```
openssl x509 -text -noout < cert2.pem
Certificate:
    Data:
        ...
        X509v3 extensions:
            ...
            X509v3 Basic Constraints: critical
                CA:TRUE
            ...
```

I couldn't find a way to have K3s issue a different certificate for each client, so I'm going to fix it by just copying
the config file:

```sh
sudo cat /etc/rancher/k3s/k3s.yaml > $KUBECONFIG
```
