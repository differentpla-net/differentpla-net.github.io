---
title: "Installing MetalLB"
date: 2021-12-20T20:55:00Z
tags: raspberry-pi
layout: series
series: k3s
---

Installation with Helm, per <https://metallb.universe.tf/installation/#installation-with-helm>.

## Add the repo

```
helm repo add metallb https://metallb.github.io/metallb
```

## Create the values file

My home network is `192.168.28.x`, my DHCP server allocates `.100` and up, so we'll use a pool of addresses outside that range.

```yaml
# values.yaml
configInline:
  address-pools:
   - name: default
     protocol: layer2
     addresses:
     - 192.168.28.10-192.168.28.40
```

## Run the installation

```bash
helm --namespace metallb-system \
    install --create-namespace \
    metallb metallb/metallb -f values.yaml
```

## Does it work?

```
$ kubectl create deployment nginx --image=nginx

$ kubectl expose deployment nginx --type=LoadBalancer --port 80

$ kubectl get services
NAME         TYPE           CLUSTER-IP      EXTERNAL-IP     PORT(S)        AGE
kubernetes   ClusterIP      10.43.0.1       <none>          443/TCP        5h53m
nginx        LoadBalancer   10.43.156.110   192.168.28.11   80:32580/TCP   117s

$ kubectl scale deployment --replicas=3 nginx
```

![](/images/2021-12-20-installing-metallb/nginx-edge.png)

<div class="callout callout-info" markdown="span">
I forgot to [disable Klipper](https://rancher.com/docs/k3s/latest/en/networking/#disabling-the-service-lb), the K3s-provided load balancer. It doesn't seem to do any harm, but I'll deal with that later.
</div>
