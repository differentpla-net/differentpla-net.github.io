---
title: "Elixir Livebook on k3s"
date: 2022-02-23T20:42:00Z
tags: k3s elixir livebook
layout: series
series: k3s
---

I'd like to run Livebook on my cluster. Here's how I went about doing that.

```
kubectl create namespace livebook
```

## deployment.yaml

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  labels:
    app: livebook
  name: livebook
  namespace: livebook
spec:
  replicas: 1
  selector:
    matchLabels:
      app: livebook
  template:
    metadata:
      labels:
        app: livebook
    spec:
      containers:
      - name: livebook
        image: livebook/livebook:0.5.2
        env:
        - name: LIVEBOOK_PORT
          value: "8080"
        - name: LIVEBOOK_PASSWORD
          valueFrom:
            secretKeyRef:
              name: livebook-password
              key: password
        - name: LIVEBOOK_ROOT_PATH
          value: /var/lib/livebook
        volumeMounts:
        - name: livebook-data-vol
          mountPath: /var/lib/livebook
      volumes:
      - name: livebook-data-vol
        persistentVolumeClaim:
          claimName: livebook-data-pvc
```

## pvc.yaml

```yaml
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: livebook-data-pvc
  namespace: livebook
spec:
  storageClassName: longhorn
  accessModes:
  - ReadWriteOnce
  resources:
    requests:
      storage: 1Gi
```

## service.yaml

```yaml
apiVersion: v1
kind: Service
metadata:
  name: livebook
  namespace: livebook

spec:
  ports:
  - name: livebook-port
    port: 8080
  selector:
    app: livebook
```

## ingress.yaml

```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress

metadata:
  name: livebook
  namespace: livebook
  annotations:
    # Traefik: secure, plz.
    traefik.ingress.kubernetes.io/router.entrypoints: websecure
    traefik.ingress.kubernetes.io/router.tls: "true"
    # Ask cert-manager to issue a TLS certificate.
    cert-manager.io/cluster-issuer: k3s-ca-cluster-issuer

spec:
  tls:
  - hosts:
      - livebook.k3s.differentpla.net
    secretName: livebook-tls
  rules:
  - host: livebook.k3s.differentpla.net
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: livebook
            port:
              number: 8080
```

## password.yaml

We need a secret to hold the password. It must be at least 12 characters, or Livebook will fail to start.

```yaml
apiVersion: v1
kind: Secret
metadata:
  namespace: livebook
  name: livebook-password
type: Opaque
data:
  # "livebook-password"
  password: bGl2ZWJvb2stcGFzc3dvcmQ=
```

## Configure DNS

We'll need to edit our [custom DNS]({% post_url 2021/2021-12-29-coredns %}):

```
kubectl --namespace k3s-dns edit configmap k3s-dns
```

```
...
data:
...
  NodeHosts: |
    192.168.28.10 livebook.k3s.differentpla.net
...
```

## Miscellanea

- Why don't I write a controller that scans for LoadBalancer and Ingress objects and updates the CoreDNS ConfigMap automatically?
- Yes, I should probably have used ArgoCD for this. It didn't occur to me until I'd already got it working. Maybe I can import it into ArgoCD?
- `LIVEBOOK_ROOT_PATH` is changed between `v0.5.2` (here) and `main`. Bear that in mind when upgrading later.
- Backups. How?
