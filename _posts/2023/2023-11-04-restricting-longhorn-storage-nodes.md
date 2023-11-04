---
title: "Restricting Longhorn to specific nodes"
date: 2023-11-04T13:03:00.000Z
tags: kubernetes longhorn
---

I've added a Raspberry Pi node to my K3s cluster, and I don't want it to take part in Longhorn replication.

On my old Raspberry Pi cluster, I had problems with corrupted Longhorn volumes, most likely due to network and storage
performance of the nodes. Since replacing the cluster hardware with a mix of Core i3, Core i5 and AMD Ryzen 5500U nodes,
I've had no problems. But as part of my over-engineered doorbell project, I've just added a new Raspberry Pi node to my
cluster, and I'd like to prevent it from taking part in anything Longhorn-related.

The Longhorn documentation suggests a number of different ways to do this, such as [taints and
tolerations](https://longhorn.io/docs/1.5.2/advanced-resources/deploy/taint-toleration/), telling Longhorn to [only use
storage on a specific set of nodes](https://longhorn.io/kb/tip-only-use-storage-on-a-set-of-nodes/), etc. My preferred
option is to use a [node selector](https://longhorn.io/docs/1.5.2/advanced-resources/deploy/node-selector/). This will
allow me to label the nodes on which I want to run Longhorn components, and then use node selectors to assign the
Longhorn workload to those nodes.

## Detaching the volumes

For this to work (it requires a restart), all Longhorn volumes must be detached. I couldn't find a good way to script
this, so I just looked at the Volumes page in the Longhorn UI and scaled down the relevant workloads.

For example:

```sh
kubectl --namespace docker-registry scale deployment docker-registry --replicas 0
```

...and so on.

I ran into a minor problem when scaling down the VictoriaMetrics agent and storage -- the operator kept scaling it back
up. So I had to scale that down first.

## Adding the node labels

The Longhorn documentation doesn't suggest any good node labels, so I made up my own.

```sh
kubectl label node roger-nuc0 differentpla.net/longhorn-storage-node=true
# ...etc.
```

## Adding the node selectors

Because I installed Longhorn using the Helm chart...

```sh
$ helm list -A | grep longhorn
longhorn                longhorn-system         10              2023-07-26 17:40:38.549430439 +0100 BST deployed        longhorn-1.5.1                  v1.5.1
```

...I need to edit the values file.

```sh
helm show values longhorn/longhorn > values.yaml
```

(I committed this to git, but couldn't push it yet -- gitea was one of the things I scaled down...)

Then I went through the `values.yaml` file and added the `nodeSelector`s. There were comments in the file explaining how
to do this, fortunately.

```yaml
# ...
longhornManager:
  # ...
  nodeSelector:
    differentpla.net/longhorn-storage-node: "true"
# ...
```

...and so on.

## Applying the changes

```sh
helm upgrade longhorn longhorn/longhorn --namespace longhorn-system --values values.yaml
```

It takes a while; it was a bit nerve-wracking.

The system-managed components also need the node selector, so once Longhorn is back up, go to the Longhorn UI, and under
`Setting > General > System Managed Components Node Selector`, enter `differentpla.net/longhorn-storage-node:true`.

## Scaling back up

Then it was just a matter of scaling the workloads back up and waiting for a bit. I originally planned to do this in
ArgoCD, which meant bringing gitea back up first:

```
kubectl --namespace gitea scale statefulset gitea-postgres --replicas 1
kubectl --namespace gitea scale statefulset gitea --replicas 1
```

Weirdly, ArgoCD only noticed that _some_ of my applications were out of sync, so I had to scale the others up manually
anyway.

For VictoriaMetrics, I only needed to scale up the vm-operator; it scaled the agent and storage pods back up itself.
