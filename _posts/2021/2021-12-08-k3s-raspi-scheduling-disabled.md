---
title: "k3s on Raspberry Pi: SchedulingDisabled"
short_title: "SchedulingDisabled"
date: 2021-12-08T09:42:00Z
layout: series
series: k3s
tags: k3s raspberry-pi
---

This morning, I grabbed [my Raspberry Pi cluster]({% post_url 2021/2021-07-04-k3s-raspi-updates %}) out of the box and fired it up again.

But my docker repository wasn't starting. I'd left it broken:

- By using the default k3s `local-path` storage class, the container was tied to a specific node.
- That node was showing as `SchedulingDisabled`:

```
$ kubectl get nodes --sort-by '.metadata.name'
NAME     STATUS                     ROLES                  AGE    VERSION
rpi401   Ready                      control-plane,master   154d   v1.21.7+k3s1
rpi402   Ready                      <none>                 154d   v1.21.7+k3s1
rpi403   Ready                      <none>                 152d   v1.21.7+k3s1
rpi404   Ready                      <none>                 152d   v1.21.7+k3s1
rpi405   Ready,SchedulingDisabled   <none>                 152d   v1.21.7+k3s1
```

```
$ kubectl get node rpi405 -o yaml
...
spec:
  ...
  taints:
  - effect: NoSchedule
    key: node.kubernetes.io/unschedulable
    timeAdded: "2021-07-10T09:41:33Z"
  unschedulable: true
...
```

It's a simple fix:

```
$ kubectl uncordon rpi405
node/rpi405 uncordoned
```

The thing that's puzzling me is that I don't remember using the `kubectl cordon` command. I _do_ vaguely remember messing around [with taints](https://kubernetes.io/docs/concepts/scheduling-eviction/taint-and-toleration/), so maybe...?
