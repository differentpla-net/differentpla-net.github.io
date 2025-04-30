---
title: "Kubernetes Taints and Tolerations"
date: 2025-04-30T13:02Z
tags: kubernetes
---

Today I learned, in a way that's going to stick, the difference between Kubernetes [Taints and
Tolerations](https://kubernetes.io/docs/concepts/scheduling-eviction/taint-and-toleration/) and [Node
Affinity](https://kubernetes.io/docs/concepts/scheduling-eviction/assign-pod-node/#affinity-and-anti-affinity).

Well, I say "today", but it was actually two almost-identical occurrences.

## Recapping nodeSelector and nodeAffinity

In Kubernetes, you can force particular pods to run on particular nodes by using `nodeSelector`. Here's how I make sure that Longhorn (which is quite resource-intensive) only runs on some of my nodes (those labelled as such):

```yaml
  nodeSelector:
    differentpla.net/longhorn-storage-node: "true"
```

Or you might want your pod to run only on x86_64 (amd64) nodes:

```yaml
  nodeSelector:
    kubernetes.io/arch: "amd64"
```

You can combine them (Longhorn storage _and_ amd64):

```yaml
  nodeSelector:
    kubernetes.io/arch: "amd64"
    differentpla.net/longhorn-storage-node: "true"
```

But you can't repeat a key, so there's no way to express "amd64 OR arm64". For that, you need `nodeAffinity`.

`nodeAffinity` is a more flexible version of `nodeSelector`. Here's how to specify which architectures a pod can run
on:

```yaml
  nodeAffinity:
    requiredDuringSchedulingIgnoredDuringExecution:
      nodeSelectorTerms:
        - matchExpressions:
          - key: kubernetes.io/arch
            operator: In
            values:
              - amd64
              - arm64
```

We're saying that the architecture must be in the `['amd64', 'arm64']` set.

Note that `nodeSelectorTerms` are OR-ed together; `matchExpressions` are AND-ed together.

Aside: make sure you don't have _both_ `nodeSelector` and `nodeAffinity`. I spent a couple of hours wondering why my
pods weren't being scheduled on ARM64 nodes -- I'd added the `nodeAffinity` as above, but not removed the `nodeSelector`

## Taints

This is great, but what if you add some nodes to your cluster, and you _don't_ want existing workloads scheduled on them?

For example: let's say you're adding arm64 nodes (at work, Graviton; at home, RPi 4) and you don't want to update all of
the existing deployments to add the missing `nodeSelector` or `nodeAffinity`.

You can add a "taint" to those nodes; Kubernetes won't schedule any workloads on the tainted nodes. Here, I'm tainted my
3 new RPi 4 nodes with `differentpla.net/arch=arm64:NoSchedule`.

```
kubectl taint nodes rpi401 rpi402 rpi403 differentpla.net/arch=arm64:NoSchedule
```

(I used `differentpla.net/arch` rather than `kubernetes.io/arch` because I didn't want the official label confused with
my custom taint).

This means that no pods will be scheduled on the tainted nodes.

## Tolerations

What if I _want_ a pod scheduled on one of those RPi 4 nodes? I've been messing around with multi-arch builds; I _want_
my builds to run on the new nodes.

That's when you add a "toleration" to the pod template. You're saying "this pod tolerates that taint; it's OK for it to
run there".

That looks like this:

```yaml
  tolerations:
    - key: differentpla.net/arch
      operator: Equal
      value: arm64
      effect: NoSchedule
```

I'm saying that this pod is allowed to run on the tainted node; it tolerates it.

As another example, you might have nodes that are flaky or experimental in some way; you don't want normal workloads to
run there, so you taint them. But you want to make use of those nodes, so you mark some low-value workloads as
tolerating that taint.
