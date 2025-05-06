---
title: "Using Kubernetes Secrets for External DNS TSIG Keys"
date: 2025-05-04T13:16Z
tags: kubernetes external-dns
---

Yesterday, I published a post about using [Kubernetes External DNS with Synology's DNS Server]({% post_url
2025/2025-05-03-k8s-external-dns-synology %}). This morning, I realised that putting the TSIG key directly in the
deployment manifest was probably a bad idea. Here's how to put it in a Kubernetes Secret object.

## Previously

Recall that, yesterday, we were left with a deployment manifest that looked like this:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: external-dns
  namespace: external-dns
spec:
  selector:
    matchLabels:
      app: external-dns
  template:
    metadata:
      labels:
        app: external-dns
    spec:
      serviceAccountName: external-dns
      containers:
        - name: external-dns
          image: registry.k8s.io/external-dns/external-dns:v0.16.1
          args:
          # ...ellided
          - --provider=rfc2136
          # DNS server IP address; this is my router.
          - --rfc2136-host=192.168.28.1
          - --rfc2136-port=53
          # The zone for the updates.
          - --rfc2136-zone=k3s.differentpla.net
          # The 'secret' from above
          - --rfc2136-tsig-secret=RGlkIHlvdSByZWFsbHkgdGhpbmsgSSdkIHB1dCB0aGUgcmVhbCBUU0lHIGtleSBoZXJlPyBIYWhhaGEgLSBOby4=
          # The example has hmac-sha256; we want hmac-sha512; make *sure* you've changed it.
          - --rfc2136-tsig-secret-alg=hmac-sha512
          # The keyname MUST match; it's part of the key.
          - --rfc2136-tsig-keyname=k3s.differentpla.net
          - --rfc2136-tsig-axfr
          - --source=ingress
          - --domain-filter=k3s.differentpla.net
```

## Environment Variables from Secrets

Change it to this:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: external-dns
  namespace: external-dns
spec:
  selector:
    matchLabels:
      app: external-dns
  template:
    metadata:
      labels:
        app: external-dns
    spec:
      serviceAccountName: external-dns
      containers:
        - name: external-dns
          image: registry.k8s.io/external-dns/external-dns:v0.16.1
          args:
            # ...ellided
            - --provider=rfc2136
            - --rfc2136-host=$(RFC2136_HOST)
            - --rfc2136-port=53
            - --rfc2136-zone=$(RFC2136_ZONE)
            - --rfc2136-tsig-secret=$(RFC2136_TSIG_SECRET)
            - --rfc2136-tsig-secret-alg=$(RFC2136_TSIG_SECRET_ALG)
            - --rfc2136-tsig-keyname=$(RFC2136_TSIG_KEYNAME)
            - --rfc2136-tsig-axfr
            # ...ellided
            - --domain-filter=$(DOMAIN_FILTER)
          env:
            - name: RFC2136_TSIG_SECRET
              valueFrom:
                secretKeyRef:
                  name: rfc2136-tsig
                  key: secret
            - name: RFC2136_TSIG_SECRET_ALG
              valueFrom:
                secretKeyRef:
                  name: rfc2136-tsig
                  key: secret-alg
            - name: RFC2136_TSIG_KEYNAME
              valueFrom:
                secretKeyRef:
                  name: rfc2136-tsig
                  key: keyname
            - name: RFC2136_HOST
              value: 192.168.28.1
            - name: RFC2136_ZONE
              value: k3s.differentpla.net
            - name: DOMAIN_FILTER
              value: k3s.differentpla.net
```

Of note here:
- We've changed some of the args to use environment variables.
- We've added an `env` section where some of those variables come from a secret.
- The other environment variables are specified in the `env` section. I could have used a `ConfigMap` for these, but
  decided not to at this point.

## Secret

Create a file as follows:

```yaml
apiVersion: v1
kind: Secret
metadata:
  name: rfc2136-tsig
  namespace: external-dns
type: Opaque
data:
  keyname: ...
  secret-alg: ...
  secret: ...
```

The values in the `data` section must be base64-encoded.

The key file exported from the Synology DNS server looked like this:

```
key "k3s.differentpla.net" {
algorithm HMAC-SHA512;
secret "RGlkIHlvdSByZWFsbHkgdGhpbmsgSSdkIHB1dCB0aGUgcmVhbCBUU0lHIGtleSBoZXJlPyBIYWhhaGEgLSBOby4=";
};
```

To generate the `keyname`, `secret-alg` and `secret` values:

```sh
echo -n "k3s.differentpla.net" | base64
echo -n "hmac-sha512" | base64
echo -n "RGlkIHlvdSByZWFsbHkgdGhpbmsgSSdkIHB1dCB0aGUgcmVhbCBUU0lHIGtleSBoZXJlPyBIYWhhaGEgLSBOby4=" | base64 -w0
```

Yes, the secret was already base64-encoded; we have to double-encode it.

Apply the secret with `kubectl apply -f secret.yaml`. Update the deployment. It should all still work.
