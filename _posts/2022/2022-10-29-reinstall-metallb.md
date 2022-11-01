---
title: "Reinstalling MetalLB"
date: 2022-10-29T10:51:00Z
tags: k3s kubernetes
layout: series
series: k3s-reinstall
---

Previous versions of MetalLB used a `ConfigMap` for address pool configuration. Newer versions use `IPAddressPool`
resources, so the installation is slightly different. We don't need a `values.yaml` file.

```
helm repo add metallb https://metallb.github.io/metallb
helm --namespace metallb-system \
    install --create-namespace \
    metallb metallb/metallb
```

## address-pool-default.yaml

My existing k3s-on-RPi cluster is still running, and is using addresses from `192.168.28.10` to `192.168.28.13`, so I'll
use a different range.

I was originally going to use a range starting at `192.168.28.20`, but then I discovered that my managed switches were
at `.21` and `.22`, so that's no good. My router's dynamic range starts at `.100`, so I'll configure MetalLB to start at
`192.168.28.60`.

And I'll put a DHCP reservation in the router for the switches, so that I don't forget them next time.

```yaml
apiVersion: metallb.io/v1beta1
kind: IPAddressPool
metadata:
  name: default
  namespace: metallb-system
spec:
  addresses:
  - 192.168.28.60-192.168.28.90
```

## l2-advertisement-default.yaml

We also need an L2 advertisement. If you don't specify a selector, then it applies to all `IPAddressPool`s. This is what
we want.

```yaml
apiVersion: metallb.io/v1beta1
kind: L2Advertisement
metadata:
  name: default
  namespace: metallb-system
```

## Apply the configuration

```
kubectl apply -f .
```

## Does it work?

```
$ kubectl create deployment nginx --image=nginx

$ kubectl expose deployment nginx --type=LoadBalancer --port 80

$ kubectl get services
NAME         TYPE           CLUSTER-IP      EXTERNAL-IP     PORT(S)        AGE
kubernetes   ClusterIP      10.43.0.1       <none>          443/TCP        5h53m
nginx        LoadBalancer   10.43.217.66    192.168.28.21   80:32580/TCP   5s

$ kubectl scale deployment --replicas=3 nginx
```

## Refreshing Services

To cause MetalLB to reload its configuration and refresh the service IP address assignments, you need to restart the
controller and speaker pods:

```
kubectl --namespace metallb-system delete pods --all
```
