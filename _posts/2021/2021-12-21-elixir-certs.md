---
title: "elixir-certs"
date: 2021-12-21T11:50:00Z
tags: elixir
---

I need to secure my Docker registry. Because OpenSSL sucks, I'm using
[an Elixir script](https://github.com/rlipscombe/elixir-certs) that uses the 'x509' library.

## Create root CA

```
./certs self-signed \
    --out-cert k3s-ca.crt --out-key k3s-ca.key \
    --template root-ca \
    --subject "/C=GB/L=London/O=differentpla.net/CN=differentpla.net k3s CA"
```

We'll be using the root CA for other certificates in future, so the certificate and key need to be copied somewhere
safe.

<div class="callout callout-info" markdown="span">
To install the root CA certificate; see [this page]({% post_url 2021/2021-12-21-install-root-cert %}).
</div>

## Create server keypair and certificate

Change `my-server` as relevant:

```
./certs create-cert \
    --issuer-cert k3s-ca.crt --issuer-key k3s-ca.key \
    --out-cert my-server.crt --out-key my-server.key \
    --template server \
    --subject '/CN=my-server.k3s.differentpla.net'
```
