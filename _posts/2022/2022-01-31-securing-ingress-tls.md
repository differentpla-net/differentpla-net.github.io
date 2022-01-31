---
title: "Securing an Ingress with TLS"
date: 2022-01-31T18:35:00Z
tags: ingress kubernetes
layout: series
series: k3s
---

My [Gitea instance]({% post_url 2022/2022-01-30-gitea %}) isn't using TLS, so I'm going to replace the LoadBalancer with an Ingress, which will allow TLS termination.

Similarly, my [docker registry]({% post_url 2021/2021-12-21-docker-registry-redux %}) currently manages its own TLS termination. It would probably be simpler to make that use an Ingress as well.

So, let's take the `whoami` [example from earlier]({% post_url 2022/2022-01-31-traefik-ingress %}) and slap a certificate on it.

## ingress.yaml

The `ingress.yaml` file from earlier needs to be changed to the following:

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress

metadata:
  name: whoami
  namespace: whoami
  annotations:
    traefik.ingress.kubernetes.io/router.entrypoints: websecure
    traefik.ingress.kubernetes.io/router.tls: "true"

spec:
  tls:
  - hosts:
      - whoami.k3s.differentpla.net
    secretName: whoami-tls
  rules:
  - host: whoami.k3s.differentpla.net
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: whoami
            port:
              number: 80
```

<div class="callout callout-info" markdown="span">
The `traefik.ingress.kubernetes.io/router.tls: "true"` annotation must use a quoted string for "true", otherwise you'll get a confusing error: `v1.ObjectMeta.Annotations: ReadString: expects " or n, but found t`
</div>

## Create server certificate

As [discussed earlier]({% post_url 2021/2021-12-21-elixir-certs %}), I'm using an Elixir script to generate certificates. Remember to install the root CA certificate, otherwise `curl`, Firefox, Chrome, etc. will complain.

```
./certs create-cert \
    --issuer-cert k3s-ca.crt --issuer-key k3s-ca.key \
    --out-cert whoami.crt --out-key whoami.key \
    --template server \
    --subject '/CN=whoami.k3s.differentpla.net'
```

```
base64 -w0 < whoami.crt
base64 -w0 < whoami.key
```

<div class="callout callout-dark" markdown="span">
**Idea:** Maybe I should just have the `certs` script output a k8s secret file...?
</div>

## tls-secret.yaml

```yaml
apiVersion: v1
kind: Secret
metadata:
  name: whoami-tls
  namespace: whoami
data:
  tls.crt: LS0tLS1...
  tls.key: LS0tLS1...
type: kubernetes.io/tls
```

### Troubleshooting

Make sure that the secret exists, otherwise Traefik uses its default certificate, "TRAEFIK DEFAULT CERT" instead. You can see this if you look in the logs:

```
$ kubectl --namespace kube-system logs traefik-786ff64748-mx9pf
time="2022-01-31T18:53:20Z" level=error msg="Error configuring TLS: secret whoami/whoami-tls does not exist" ingress=whoami providerName=kubernetes namespace=whoami
```

## Links

- <https://kubernetes.io/docs/concepts/services-networking/ingress#tls>
