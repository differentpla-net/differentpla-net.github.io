---
title: "Using k3s on Raspberry Pi: More"
date: 2021-12-10T23:59:00Z
layout: series
series: k3s
tags: k3s raspberry-pi
---

- What's the difference between port-forward and proxy?
  - Particularly in the context of the dashboard.
- Node anti-affinity: how do I dissuade a pod from being on the same node as another pod?
- Play with DaemonSets.
- Play with StatefulSets.
  - We (might) want them to provide some kind of consistent "name" to a pod, so that it "owns" some data.
  - That data is not (necessarily) _in_ the pod. That is: it's a record in a database, not a PV.
- Databases?
  - Everyone seems to be "don't use k8s for your database", but it's gotta go _somewhere_.
- Redis sentinel in a k8s cluster. How reliable is it?
  - Can we ignore it from the pov of a client, if it's all hidden behind a service?
- How best to manage k8s yaml files?
- CI, using, e.g., ArgoCD. ArgoCD because Twilio seems to be leaning that way. What others are available, and why not use those?
- What does `ReclaimPolicy` on a `PersistentVolume` do? How can I be sure that my data's getting/not getting deleted appropriately?
- Getting MetalLB to update DNS.
  - <https://particule.io/en/blog/k8s-no-cloud/>
  - Requires `external-dns`, which has a _small_ list of supported DNS providers.
  - Rabbit hole.
- Keep needing to type `--namespace`: https://kubernetes.io/docs/reference/kubectl/cheatsheet/ -- use contexts. Not doing that yet.
- Local S3 server: https://github.com/sleighzy/k3s-minio-deployment, which has
  other useful things in it.
- What else can we learn from https://rpi4cluster.com/?

## Redeploying (TODO)

Because of the PVC, which can only be used by one pod at a time, you can't redeploy without deleting the existing pod. Is there a way to get k8s to tear the old one down first? That doesn't work either: I need to delete the old deployment first?

That's ... interesting.

How do you prevent the PV from being deleted? It's got ReclaimPolicy=Delete, but the PVC didn't get deleted, so maybe we're good? Seems dangerous.

## How can we tell whether a service is bound to a pod?

`kubectl describe service my-service` lists endpoints. If there are none, then the service isn't backed by an app.

- Cross-compiling arm64 containers on arm64/x86_64 PCs? Because Pi4 is kinda slow.
