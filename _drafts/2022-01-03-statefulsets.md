---
title: "StatefulSets"
date: 2022-01-03T15:14:00Z
tags: k3s
---

Let's play with StatefulSets.

I've been told that they can be used to give a node a consistent name. This appears to be true, but they also imply an ordering of the nodes.

From "Cloud Native DevOps with Kubernetes" (O'Reilly):

> What a StatefulSet adds is the ability to start and stop pods in a specific sequence.

This is ... not what I want for (work-related thing), but I'm gonna play with it anyway.

The k8s documentation is more balanced:

> StatefulSets are valuable for applications that require one or more of the following.
> - Stable, unique network identifiers.
> - Stable, persistent storage.
> - Ordered, graceful deployment and scaling.
> - Ordered, automated rolling updates.

- Headless services
- Rolling updates, etc.

The yaml I've got doesn't have any storage; this is fine for my purposes. I need to dig into deployment and scaling.

Honestly, though, the "ordered" thing worries me. There are a bunch of warnings about deploys -- it'll only scale up
or down by deleting the predecessor, successor. Is that a problem? Might not be; if they're just numbers, rather than
actually stateful, that's no big deal.

Still think that the correct answer, Erlang clustering or not, is to separate the workload scheduler into a different, HA/sharded/whatever pod, and have it distribute the workload over an arbitrary replicaset (i.e. a deployment).

## References

- <https://kubernetes.io/docs/concepts/workloads/controllers/statefulset/>
