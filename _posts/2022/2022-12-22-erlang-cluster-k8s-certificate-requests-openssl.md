---
title: "Erlang cluster on Kubernetes: Creating Certificate signing requests with OpenSSL"
short_title: "Creating CSRs with OpenSSL"
date: 2022-12-22T09:58:00.000Z
layout: series
series: erlang-cluster-k8s
tags: openssl
---

Erlang TLS distribution is picky about how its certificates are created. OpenSSL is a pain to use. Here's how to
awkwardly wedge the two together. This took me several hours and a lot of my sanity. You're welcome.

## Private key

First, we need a private key. I'm going to use an EC key. _cert-manager_ is picky about which curves it accepts, so
stick with `prime256v1`.

```bash
openssl ecparam -name prime256v1 -genkey -noout -out "$CERTS_DIR/$CERT_FILENAME.key"
```

`CERTS_DIR` and `CERT_FILENAME` are passed in from the K8s deployment and point to the `/certs` volume that's shared
between the two containers.

## Configuration file

You _can_ create a certificate signing request without an OpenSSL config file, but if you want to specify any
extensions, you're going to need one. We do, so we do.

```bash
cat <<EOF > "$CERTS_DIR/$CERT_FILENAME.cnf"
[req]
req_extensions = req_extensions
distinguished_name = req_distinguished_name

[req_distinguished_name]

[req_extensions]
subjectAltName = @alt_names
extendedKeyUsage = serverAuth,clientAuth

[alt_names]
DNS = ${MY_POD_IP}
EOF
```

We use a heredoc because we need to substitute `${MY_POD_IP}` into one of the alternate names.

I guess we could have used a template file and `sed` (or `envsubst`).

## Creating the signing request

```bash
openssl req -new \
            -key "$CERTS_DIR/$CERT_FILENAME.key" \
            -subj "/CN=${MY_POD_IP}" \
            -out "$CERTS_DIR/$CERT_FILENAME.csr" \
            -config "$CERTS_DIR/$CERT_FILENAME.cnf"
```

This generates a CSR file, e.g. `/certs/tls-dist.csr`. You can submit that to your certificate authority and you should
get a signed certificate back.

As discussed at the in the introduction to this series, I'm going to use _cert-manager_. See the next post.
