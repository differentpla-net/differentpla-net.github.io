---
title: "Upgrading k3s"
date: 2022-01-03T14:38:00Z
tags: k3s
---

The basic plan is to drain each node in turn, apply the upgrade and then uncordon the node. I'm upgrading from `v1.21.7+k3s1` to `v1.22.5+k3s1`.

Because I'm running Longhorn, it's a little bit more complicated.

## Worker Nodes

On the master:

```
kubectl drain rpi405 --ignore-daemonsets --pod-selector='app!=csi-attacher,app!=csi-provisioner'
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

## Control Plane

This is a non-HA cluster (I've only got one server node), and `kubectl drain rpi401` doesn't want to evict pods with local storage. So I guess I just apply the upgrade and hope that it recovers after a restart:

```
sudo apt update
sudo apt upgrade
curl -sfL https://get.k3s.io | sh -
```

## error: cannot delete Pods

I had a spare, temporary, busybox node kicking around (from `kubectl run`):

```
error: cannot delete Pods not managed by ReplicationController, ReplicaSet, Job, DaemonSet or StatefulSet (use --force to override): default/busybox
```

Confirm that it's unused, then just delete it:

```
kubectl delete pod busybox
```
