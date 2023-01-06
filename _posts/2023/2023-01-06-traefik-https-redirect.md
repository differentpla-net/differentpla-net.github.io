---
title: "Redirecting HTTP to HTTPS with Traefik on K3s"
date: 2023-01-06T16:57:00.000Z
tags: traefik kubernetes k3s
---

I want visitors to `http://home.k3s.differentpla.net` to be redirected to `https://home.k3s.differentpla.net`. Here's
how to set that up on K3s, using Traefik middlewares.

I don't want to enable redirection globally, because I suspect that K3s will revert the change when I next upgrade, so
I'm going to do it on each `Ingress` and `IngressRoute`.

First, we need to check that `--providers.kubernetescrd` is enabled:

```sh
kubectl --namespace kube-system \
    get deployment traefik -o yaml | \
    yq '.spec.template.spec.containers.0.args' | \
    grep -- '--providers.kubernetescrd'
```

It's there, so it's enabled.

## HTTP ingress

Then we need to add another Ingress object and some Middleware:

```yaml
apiVersion: traefik.containo.us/v1alpha1
kind: Middleware
metadata:
  name: k3s-home-redirect
  namespace: k3s-home
spec:
  redirectScheme:
    scheme: https
    permanent: true
---
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: k3s-home-redirect
  namespace: k3s-home
  annotations:
    traefik.ingress.kubernetes.io/router.entrypoints: web
    # MUST be <namespace>-<name>@kubernetescrd
    traefik.ingress.kubernetes.io/router.middlewares: k3s-home-k3s-home-redirect@kubernetescrd
spec:
  rules:
  - host: home.k3s.differentpla.net
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: k3s-home
            port:
              name: http
```

Somewhat annoyingly, it appears that we need to copy-and-paste the rules from the [original Ingress]({% post_url 2022/2022-10-15-kustomize-nginx %}),
otherwise `kubectl apply` fails.

Seems to work though. Which is nice.

## Related Links

- <https://community.traefik.io/t/http-to-https-redirection-on-k8s-ingress/8702/3>
