---
title: "Upgrading Longhorn: Incident Review"
date: 2023-11-05T21:04:00Z
tags: kubernetes longhorn incident-review
---

A new release of Longhorn came out a few days ago. I tried upgrading. It did not go well. This is the incident review.

On November 5th, 2023 between approximately 18:05 and 20:45, immediately after an upgrade, Longhorn decided that all of
the nodes in my K3s cluster were unschedulable.

This blog post started as a write-up of the upgrade procedure. That upgrade went sideways, and turned into a 2h40
outage and this incident report.

## Background

The Longhorn UI told me that there was an upgrade from my running 1.5.1 version of Longhorn. Looking on the Github
releases page, I saw a 1.5.2 release and decided to upgrade to it.

<div class="callout callout-info" markdown="span">
I was **not** actually running 1.5.1. The Longhorn UI is potentially confusing.
</div>

## Initial upgrade attempt

At approximately 18:06, I attempted to upgrade Longhorn by running the following commands:

```sh
helm repo update
helm upgrade longhorn longhorn/longhorn --namespace longhorn-system --values values.yaml
```

This failed with the following error:

```
Error: UPGRADE FAILED: pre-upgrade hooks failed: job failed: BackoffLimitExceeded
```

## Failed jobs?

This [issue](https://github.com/helm/helm/issues/6873) -- which is allegedly fixed, but... -- suggests that I've got some failed jobs somewhere in my cluster.

Running `kubectl get pods -A | grep -v Running` showed that I had about half a dozen `CronJob` pods in the
`ContainerStatusUnknown` state, so I deleted those. This did _not_ resolve the problem.

## Pre-upgrade

This completely unrelated
[page](https://docs.microfocus.com/doc/Containerized_Operations_Bridge/2023.05/BackoffLimitExceeded) suggested that the
chart runs some kind of pre-upgrade job, so I checked for that:

```
kubectl --namespace longhorn-system get events
```

In the events, I saw the following:

```
82s         Normal    SuccessfulCreate       job/longhorn-pre-upgrade         Created pod: longhorn-pre-upgrade-rc9rr
82s         Normal    Scheduled              pod/longhorn-pre-upgrade-rc9rr   Successfully assigned longhorn-system/longhorn-pre-upgrade-rc9rr to roger-bee2
80s         Normal    Pulled                 pod/longhorn-pre-upgrade-rc9rr   Container image "longhornio/longhorn-manager:v1.4.2" already present on machine
80s         Normal    Created                pod/longhorn-pre-upgrade-rc9rr   Created container longhorn-pre-upgrade
80s         Normal    Started                pod/longhorn-pre-upgrade-rc9rr   Started container longhorn-pre-upgrade
79s         Warning   BackOff                pod/longhorn-pre-upgrade-rc9rr   Back-off restarting failed container longhorn-pre-upgrade in pod longhorn-pre-upgrade-rc9rr_longhorn-system(f3a4cd0b-b410-467b-ab23-6333bcc192dd)
78s         Normal    SuccessfulDelete       job/longhorn-pre-upgrade         Deleted pod: longhorn-pre-upgrade-rc9rr
78s         Warning   BackoffLimitExceeded   job/longhorn-pre-upgrade         Job has reached the specified backoff limit
```

Because this is a deleted pod, there's no good way to get the logs from it.

## Which version?

What is puzzling, however, is that it said `Container image "longhornio/longhorn-manager:v1.4.2" already present on
machine`. At the time I thought I was upgrading from `v1.5.1` to `v1.5.2`. It shouldn't be pulling `v1.4.2`.

`helm list` showed the expected chart version (and that it failed):

```
$ helm list -n longhorn-system
NAME            NAMESPACE       REVISION        UPDATED                                 STATUS  CHART           APP VERSION
longhorn        longhorn-system 15              2023-11-04 17:00:16.470222235 +0000 UTC failed  longhorn-1.5.2  v1.5.
```

I rendered the Helm chart locally...

```sh
helm template longhorn/longhorn --output-dir .
```

...and looked through it. It's definitely supposed to be using `v1.5.2`.

## Wrong values in `values.yaml`

When I configured the [nodeSelector]({% post_url 2023/2023-11-04-restricting-longhorn-storage-nodes %}) values for Longhorn yesterday, I created the `values.yaml` file as follows:

```sh
# yesterday, before updating the repo
helm show values longhorn/longhorn > values.yaml
```

It turns out that the `values.yaml` file has image tags listed in it. Because I generated the values file before running
`helm repo update`, it had the v1.4.2 settings in it, including the image tags.

So I created a new `values.yaml` file with v1.5.2:

```sh
# after updating the repo
helm show values longhorn/longhorn > values.yaml
```

...and tried the upgrade again:

```sh
helm upgrade longhorn longhorn/longhorn --namespace longhorn-system --values values.yaml
```

## Missing nodeSelector entries

While monitoring the pods, I noticed that some of them were running on the Raspberry Pi node. When I re-created the `values.yaml` file (above), I forgot to include the `nodeSelector` entries to restrict the set of Longhorn-using nodes.

So I put them back (the list of Longhorn components had also changed), and ran the upgrade again:

```sh
helm upgrade longhorn longhorn/longhorn --namespace longhorn-system --values values.yaml
```

However, the documentation has this warning in it:

> Don’t operate the Longhorn system while node selector settings are updated and Longhorn components are being restarted.

It's unclear to me whether this caused the problem, or whether it was the upgrade, but it was a bad idea.

## Unschedulable Nodes

At this point, the Longhorn UI showed that all of my volumes were detached, and that there were no schedulable nodes.
The pods were still running happily.

To avoid any data corruption, I scaled the Longhorn-using workload back down (in no particular order, except where
shown):

```sh
kubectl --namespace livebook scale deployment livebook --replicas 0
kubectl --namespace docker-registry scale deployment docker-registry --replicas 0

# stop gitea before its database
kubectl --namespace gitea scale statefulset gitea --replicas 0
kubectl --namespace gitea scale statefulset gitea-postgres --replicas 0

kubectl --namespace vault scale statefulset vault --replicas 0
kubectl --namespace grafana scale deployment grafana --replicas 0
kubectl --namespace grafana scale deployment tempo-minio --replicas 0

# stop the VM operator first, otherwise it restarts the agent and storage.
kubectl --namespace monitoring-system scale deployment vm-operator --replicas 0
kubectl --namespace monitoring-system scale deployment vmagent-default --replicas 0
kubectl --namespace monitoring-system scale deployment vmsingle-default --replicas 0
```

Then I edited the `values.yaml` file to trim it down to _just_ the `nodeSelector`s, and tried _again_.

Still no joy.

## Restarting Longhorn

I tried to restart Longhorn, by scaling down (and back up) the deployments, and by doing a rolling restart of the
daemonsets.

This didn't resolve the problem.

## Unknown Disk Type?

Going to the `Edit Node and Disks` page for a node, I could see that the default disk for that node displayed a red exclamation mark, saying that the disk wasn't schedulable or ready. It said something about the disk type not being detected.

By running `kubectl --namespace longhorn-system get events | grep Warning`, I could see the same:

```
31m         Warning   Ready                    node/roger-bee2    Disk default-disk-27afb7a391d5e615(/var/lib/longhorn/) on node roger-bee2 is not ready: failed to get disk config: error: unknown disk type
31m         Warning   Schedulable              node/roger-bee2    the disk default-disk-27afb7a391d5e615(/var/lib/longhorn/) on the node roger-bee2 is not ready
29m         Warning   Ready                    node/roger-nuc0    Disk default-disk-279c062d423c12e8(/var/lib/longhorn/) on node roger-nuc0 is not ready: failed to get disk config: error: unknown disk type
29m         Warning   Schedulable              node/roger-nuc0    the disk default-disk-279c062d423c12e8(/var/lib/longhorn/) on the node roger-nuc0 is not ready
```

## Dinner

At about 19:00, I went for dinner with the family, which took about 30-40 minutes.

## Rolling back the chart

I tried to roll back the Helm chart, but it did literally nothing. I reapplied the upgrade and it did nothing.

## Recovery

I looked in `/var/lib/longhorn` on one of the nodes, and as far as I could tell, it looked fine. The node needed a
restart, so I tried that.

Shortly after the node came back up, I decided to create a new disk on the node, in `/var/lib/longhorn2`, to see if
there was any obvious difference (maybe a marker file had got corrupted or gone missing, or something).

<div class="callout callout-success" markdown="span">
The node recovered.
</div>

It wasn't clear to me whether it was creating a new disk that fixed the problem, or whether that was a coincidence and it was the node restart.

I drained and restarted another node and left it for ~10 minutes while I watched a YouTube video.

That didn't resolve the problem, so I went through each of the remaining nodes, creating a new disk, waiting for a
minute or so, and then deleting it again. This took a few minutes for each node, so about 10-15 minutes.

This resulted in all of the nodes springing back to life.

I scaled the workload back up, which took about another 5 minutes or so.

## Completing the upgrade

The final step in a Longhorn upgrade is upgrading the "engine image" for each volume. This can be done while the
workload is online, so I did that.

## Further investigation

I was still puzzled about which version I'd upgraded from. Here's the evidence:

- The available engine images were `v1.4.1` and `v1.5.2`, and no others. This strongly suggests that I was actually
  running `v1.4.1`.
- The chart values had `v1.4.2` image tags. The only way I can resolve this is to assume that I ran `helm repo update`
  shortly after `v1.4.2` was released, probably for some other chart, and never got around to actually upgrading from
  `v1.4.1`.
- The list of historical replicasets in the `longhorn-system` namespace have ages of `102d` and `120d` (and further
  back).
  - `v1.4.2` was released on May 12th, and `v1.4.1` on March 13th. `102d` ago would be July 26th, and `120d` ago
  would be July 8th.
  - `v1.5.1` was released on July 19th.
  - This doesn't particularly clarify things.

## Learnings

- Trim values.yaml files to just the essential pieces; the defaults will come from the template, and they might change a lot between versions.
- The list of things to apply nodeSelector to changed. Recreate (and review) values.yaml when upgrading a chart.
- Longhorn tolerates a NoSchedule taint, so cordoning the node isn't sufficient to prevent it running.
- Did reapplying the nodeSelectors with attached volumes cause the incident, or was it the upgrade?
  - Probably no way to tell. I'm not going to try to replicate the issue.
- Should I scale down the Longhorn-using workloads when upgrading it?
- Honourable mention: I'm running WSL2 on my laptop. It suffers from substantial clock drift. There are several open
  (and closed) bugs about this issue. This made getting an accurate timeline much harder, because `history -E` had the
  wrong timestamps in it.
