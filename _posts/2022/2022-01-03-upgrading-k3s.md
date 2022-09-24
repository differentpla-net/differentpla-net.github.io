---
title: "Upgrading k3s"
date: 2022-01-03T14:38:00Z
layout: series
series: k3s
tags: k3s
---

<div class="callout callout-info" markdown="span">
The Rancher docs say that you should update the server first, then the workers.
</div>

## Control Plane

This is a non-HA cluster (I've only got one server node), and `kubectl drain rpi401` (the server) doesn't want to evict
pods with local storage. So I guess I just apply the upgrade and hope that it recovers after a restart:

```
sudo apt update
sudo apt upgrade
curl -sfL https://get.k3s.io | sh -   # but see below, re: Klipper
```

## Disabling Klipper

A few hours later, I realised that I forgot to [disable Klipper]({% post_url 2021/2021-12-21-disabling-klipper %}). I've
not gone back and tried again, but based on the documentation (see below for links), I'm going to assume that the
following is the correct incantation for the installation script:

```
curl -sfL https://get.k3s.io | INSTALL_K3S_EXEC="--disable servicelb" sh -
```

- [Disabling Klipper]({% post_url 2021/2021-12-21-disabling-klipper %})
- [Rancher Docs: Networking: Disabling the Service LB](https://rancher.com/docs/k3s/latest/en/networking/#disabling-the-service-lb)
- [Rancher Docs: Installation Options: INSTALL_K3S_EXEC](https://rancher.com/docs/k3s/latest/en/installation/install-options/how-to-flags/#example-b-install-k3s-exec)

The basic plan is to drain each node in turn, apply the upgrade and then uncordon the node. I'm upgrading from `v1.21.7+k3s1` to `v1.22.5+k3s1`.

## Worker Nodes

On the master:

```
kubectl drain rpi405 \
    --ignore-daemonsets \
    --pod-selector='app!=csi-attacher,app!=csi-provisioner' # because of Longhorn
```

On the worker (rpi405, in this example):

```
sudo apt update
sudo apt upgrade
curl -sfL https://get.k3s.io | K3S_URL=https://rpi401:6443 K3S_TOKEN=K... sh -
```

The above command line was in bash history, so I didn't need to dig out the token. It's in `/var/lib/rancher/k3s/server/node-token` on the server node, if you need it.

On the master:

```
kubectl uncordon rpi405
```

Repeat for each worker.

## error: cannot delete Pods

I had a spare, temporary, busybox node kicking around (from `kubectl run`):

```
error: cannot delete Pods not managed by ReplicationController, ReplicaSet, Job, DaemonSet or StatefulSet (use --force to override): default/busybox
```

Confirm that it's unused, then just delete it:

```
kubectl delete pod busybox
```
