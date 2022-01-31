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

## Install root CA

### Ubuntu

You need to do the following on every node (and on any other host that needs to push to the registry):

```
sudo mkdir -p /usr/local/share/ca-certificates/differentpla.net
sudo cp k3s-ca.crt /usr/local/share/ca-certificates/differentpla.net/differentpla.net_K3S_CA.crt
sudo update-ca-certificates
```

### Chrome

1. Go to Settings -> Security and Privacy -> Security -> Manage Certificates -> Authorities.
2. Click on "Import"; select the `k3s-ca.crt` file.
3. Ensure that "Trust this certificate for identifying websites" is checked.
4. Click OK.

### Firefox

1. Go to Settings -> Privacy & Security -> Certificates -> View Certificates -> Authorities.
2. Click on "Import"; select the `k3s-ca.crt` file.
3. Ensure that "Trust this CA to identify web sites" is checked.
4. Click OK.

### Other operating systems and browsers

Left as an exercise for the reader.

## Create server keypair and certificate

Change `my-server` as relevant:

```
./certs create-cert \
    --issuer-cert k3s-ca.crt --issuer-key k3s-ca.key \
    --out-cert my-server.crt --out-key my-server.key \
    --template server \
    --subject '/CN=my-server.k3s.differentpla.net'
```
