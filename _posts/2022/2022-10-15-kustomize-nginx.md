---
title: "Using kustomize to configure nginx to serve static HTML"
short_title: Kustomize and nginx
date: 2022-10-15T14:37:00Z
tags: k3s kubernetes kustomize
layout: series
series: k3s
---

As I add more things to my k3s cluster, I find myself wishing that I had a handy index of their home pages. For example,
I've got ArgoCD and Gitea installed. I probably want to expose the Longhorn console, and the Kubernetes console. I think
Traefik has a console, too. I'll also be adding Grafana at some point soon.

I decided to add a static HTML page with a list of useful links in it. I'll host it in nginx.

## Deployment

We'll need a deployment:

```yaml
apiVersion: apps/v1
kind: Deployment

metadata:
  name: k3s-home
  namespace: k3s-home
  labels:
    app: k3s-home

spec:
  replicas: 2
  selector:
    matchLabels:
      app: k3s-home
  template:
    metadata:
      labels:
        app: k3s-home
    spec:
      containers:
      - name: k3s-home
        image: nginx
```

This isn't the final version; we'll need to add a ConfigMap to hold the static HTML files.

## Service

We need a basic service:

```yaml
apiVersion: v1
kind: Service

metadata:
  name: k3s-home
  namespace: k3s-home

spec:
  ports:
  - name: http
    port: 80
  selector:
    app: k3s-home
```

## Ingress

I'm using Traefik, so I'll use an Ingress. Note that it's configured for TLS:

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress

metadata:
  name: k3s-home
  namespace: k3s-home
  annotations:
    traefik.ingress.kubernetes.io/router.entrypoints: websecure
    traefik.ingress.kubernetes.io/router.tls: "true"
    cert-manager.io/cluster-issuer: k3s-ca-cluster-issuer

spec:
  tls:
  - hosts:
      - home.k3s.differentpla.net
    secretName: k3s-home-tls
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

I'm [using ArgoCD]({% post_url 2022/2022-02-02-argocd %}), so I created a new repo on my Gitea instance, and created an
ArgoCD application. I hit the Sync button and waited for about a minute.

I added a new entry to my [k3s-dns configuration]({% post_url 2021/2021-12-29-coredns %}), synced that, and waited some
more.

Then, when I browsed to the relevant URL, the default nginx welcome page appeared.

## Add the ConfigMap

In `deployment.yaml`, add `volumeMounts` (under `container`) and `volumes` (under `spec`) sections as follows:

```yaml
# ...
    spec:
      containers:
      - name: k3s-home
        image: nginx
        volumeMounts:
        - name: nginx-html
          mountPath: /usr/share/nginx/html/
      volumes:
      - name: nginx-html
        configMap:
          name: nginx-html
```

Any files you put in the `nginx-html` ConfigMap will be mounted in the volume, and nginx will serve them.

## Kustomize

I want to inject the HTML (and maybe CSS) for the home page using a ConfigMap, but I don't want to edit HTML directly in
a `configmap.yaml` file.

Fortunately, you can use Kustomize for that.

To do this, create a `kustomization.yaml` file:

```yaml
apiVersion: kustomize.config.k8s.io/v1beta1
kind: Kustomization

namespace: k3s-home

resources:
  - deployment.yaml
  - service.yaml
  - ingress.yaml

configMapGenerator:
  - name: nginx-html
    files:
    - html/index.html
```

ArgoCD will automatically apply it. Make sure that you list any files that you want kustomized under `resources`. In
this case, the only important one is `deployment.yaml`, but I listed all of them, just in case.

Kustomize will generate a ConfigMap containing the files specified, and will name it `nginx-html-<someSuffix>`. It will
then fix up the deployment to refer to the correct suffix.

Note that you need the namespace, because otherwise Kustomize messes up the above step, and doesn't fix up the
deployment correctly. See [kustomize#1301](https://github.com/kubernetes-sigs/kustomize/issues/1301).

## Links

- <https://scriptcrunch.com/change-nginx-index-configmap/>
- <https://stackoverflow.com/questions/63573289/can-i-get-a-configmap-value-from-an-external-file>
- <https://stackoverflow.com/questions/56210674/kubernetes-deployment-is-missing-kustomizes-hash-suffixes>
- <https://github.com/kubernetes-sigs/kustomize/issues/1301>
