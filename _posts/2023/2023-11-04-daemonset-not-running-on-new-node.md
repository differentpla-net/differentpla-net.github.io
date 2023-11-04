---
title: "Daemonset not running on new node"
date: 2023-11-04T12:47:00.000Z
tags: kubernetes
---

As part of my over-engineered doorbell project, I've added a new Raspberry Pi node to my K3s cluster, but none of the
daemonsets are being scheduled onto it.

Specifically, I have `7` nodes, and all of my daemonsets are showing `desiredNumberScheduled: 6`.

Running `kubectl get node doorbell -o yaml` shows that the node has a `NoSchedule` taint:

```yaml
  taints:
  - effect: NoSchedule
    key: node.cloudprovider.kubernetes.io/uninitialized
    value: "true"
```

This is weird; I've never seen this with K3s before. All of the things I can find on the Internet talk about a Cloud
Provider. But I'm not using one of those...?

I don't see anything obvious in `journalctl -u k3s-agent`.

I have no idea whether this is the correct thing to do or not, but I manually removed the taint:

```sh
kubectl taint node doorbell node.cloudprovider.kubernetes.io/uninitialized=true:NoSchedule-
```

...and things started being scheduled to the node.

Because the new node is a Raspberry Pi, and it's relatively underpowered, I probably want to explore taints and
tolerations in more depth, to avoid running anything except essential pods on it.
