---
title: "K3s PV loss: Incident Review"
date: 2022-12-15T10:37:00.000Z
tags: k3s incident-review
---

On December 14th, 2022 at approximately 10:15, Kubernetes wiped the persistent volumes of a number of applications in
the K3s cluster in my homelab. This is the incident review.

## Background

My K3s cluster consists of 3 nodes: `roger-nuc0` (the server), `roger-nuc1` and `roger-nuc3`. `roger-nuc2` doesn't exist
yet.

I use ArgoCD and Gitea for GitOps. The cluster contains a Docker registry. I'm using VictoriaMetrics and Grafana for
monitoring. I use Longhorn for persistent volumes and MetalLB for load balancers.

## Timeline

All times are in UTC (I'm in London).

December 14th:

- 10:00: Applied Ubuntu upgrades and rebooted roger-nuc0.
- 10:04: Applied Ubuntu upgrades and rebooted roger-nuc3.
- 10:16: Upgraded K3s on roger-nuc0.
- 10:17: Applied Ubuntu upgrades and rebooted roger-nuc1.
- 10:20 (approx): Upgraded K3s on roger-nuc1 and roger-nuc3.
- 10:30-18:00: Got on with day job.

December 15th:
- 09:00 (approx): Noticed that I could no longer access grafana dashboards. It was prompting for login credentials and
  would not accept my username and password.
- 09:30 (approx): Logged into grafana with default `admin`/`admin` credentials and reset the password.
  All dashboards gone. Not yet a big deal: only one simple dashboard needed to be recreated; the others were imported.
- 09:45 (approx): Rebuilt the dashboard, noticed that the earliest metrics visible are from Dec 14th at 10:10. This puts
  a timestamp on when the data was lost.
- 10:00 (approx): Looked in Longhorn console to see if there were any faults reported against the VictoriaMetrics or
  Grafana volumes. No; they're all about 1-2 months old, as expected.
- 10:00 (approx): Looked in ArgoCD (it's the first link on the cluster home page), to discover that it was stuck
  refreshing application status. Clicking on an application caused it to report that it couldn't access the git
  repository. The same was reported for all applications.
- 10:00 (approx): Looked in Gitea, which _seemed_ OK. It was showing repositories, history summary, etc.. However,
  clicking on any repository resulted in an error about not being able to find that repository.
- 10:20: The problem appears to be isolated to _some_ Longhorn volumes -- the gitea database appears intact, for
  example.
- 10:30: Started thinking about restoring from backup. Discovered that I've only got retention for 1 backup, and that
  the backup ran at midnight last night. This means that it's probably backed up the blank volumes. Fortunately, there
  exists a manual backup from mid-November for each volume, and I've done very little to the cluster since then, so that
  might be sufficient.
- 10:40: Scaled down the gitea deployment to avoid further data loss.
- 10:45: Had a cup of tea and a think.
- 11:00: Started writing a proper incident report (this).
- 11:17: My local docker registry appears to be intact.
- 11:30: Failed to find out _why_ the volumes were wiped.
- 12:00: Had a cup of tea and a think.
- 13:00: Reverted to the November snapshot of the Gitea volumes. See below.

## Things that went poorly

- Needed to refresh my memory about how to scale down statefulsets, since they're not deployments.
- Didn't know where to find K3s logs: `journalctl -u k3s` and `journalctl -u k3s-agent`.
- The instructions for mounting longhorn volumes don't seem to work. If I attach the volume to a node, the
  `/dev/longhorn` entries aren't correctly created.
  - It appears that you should _not_ check 'Maintenance' mode.
- Minimized Ubuntu installation is _really_ minimized. No `less`, for example. Consider unminimizing.
- Confusion about restoring Longhorn backups vs. reverting to snapshots.

## Immediate Actions

### Revert to the November Gitea snapshot

Ensure the pods are all stopped:

```
kubectl --namespace gitea scale statefulset gitea --replicas 0
kubectl --namespace gitea scale statefulset gitea-postgres --replicas 0
```

I was originally planning to restore from backup, so I deleted the PV:

```
kubectl delete pv pvc-7e8... --grace-period=0 --force
kubectl patch pv pvc-7e8... -p '{"metadata": {"finalizers": null}}'
```

Revert to the snapshot:

In the Longhorn UI:

1. Attach the two volumes in maintenance mode.
2. Navigate to the volume details and revert to the relevant snapshot.
3. Detach the two volumes.

Restart the pods:

```
kubectl --namespace gitea scale statefulset gitea --replicas 1
kubectl --namespace gitea scale statefulset gitea-postgres --replicas 1
```

The `gitea-0` pod didn't start because the PV didn't exist. So, in the Longhorn UI, click on the hamburger menu for the
volume and select Create PV/PVC to recreate it.

The pod then starts.

All of the data appears to be there, albeit from last month rather than yesterday.

### Longhorn: Increase backup retention count

Because the only retained automatic backup occurred _after_ the data loss, the backup is empty. Increasing the retention
count would mitigate this. Since the backups are kept in S3, which costs money, the exact number to be decided. I'll use
3 for now.

I also changed the backup frequency from daily to every second day. This trades off the retention period of the backups
(a week) and the size/count of the backups (3) against the risk of less frequent backups (two days at risk, rather than
one).

## Root Cause

Root causes are unnecessarily reductive, but in this case: shrug, who knows? Something in Kubernetes fucked up.

## Further Actions

### Avoid big-bang upgrades

Upgrading Ubuntu and K3s on all cluster nodes at the same time made it hard to determine the cause of the problem.

In future, perform upgrades separately from each other.

### Take snapshots or backups before upgrades

If I'd taken a snapshot before performing the upgrade, I wouldn't need to revert all the way back to November. This
ought to be addressed by more frequent snapshots and by increased backup retention, describe below.

### Drain nodes before upgrading and restarting

I didn't bother draining the nodes as I was performing the upgrades and restarts. I _suspect_ that this might have
triggered the situation where Kubernetes wiped the persistent volumes. Consider draining the nodes properly in future.

### Longhorn: Long-term retention of backups

There doesn't seem to be an obvious way to configure Longhorn to keep daily/weekly/monthly backups. For example: keeping
the last 7 days, the last 4 week and the last 6 months.

### Longhorn: investigate using local snapshots

Since the problem appears to be caused by the pod reinitialising the volume on startup, volume snapshots would make
recovery easier. It should be possible to configure Longhorn to take more frequent local snapshots.

### Longhorn: Recurring jobs are ambiguous

It's not obvious in the UI how jobs are associated with volumes. I created a snapshot job that _appears_ to apply to all (or no) volumes.

I've got recurring backup jobs for each volume, but that's not obvious in the UI.

### Storage: investigate using local Synology DiskStation

I've got a DS211 and a DS416 which could be used for either backups or as an iSCSI target. Longhorn (as far as I can
tell) only supports backing up to S3, so I'd need to install minio somewhere in the home lab and back _that_ up to the
DiskStation. Alternatively, I could get a newer DiskStation which supported Docker containers (neither of mine do) and
run minio directly on it.

Or I could stop using Longhorn entirely and use the DiskStation as an iSCSI target, and store persistent volumes there
instead.

### ArgoCD: Backups

ArgoCD keeps its data as Kubernetes resources. While no ArgoCD data was lost, I realised that it's not being backed up.
Address this.

### Learning

Do some investigation to answer the following questions:

- When does Kubernetes decide a volume needs a filesystem?
- Is that logged anywhere?
- Was it the node restart or the K3s upgrade that wiped the filesystem?

### Test the backups

You don't have a backup unless you can restore it. I should test the backups more frequently (as in: at all).

### Alerting

Is there any way that I could have spotted the wiped volumes earlier (before it backed up the blank data)? Obviously,
this problem impacted the monitoring system, so that's not gonna work.

But, for example, if I were displaying the dashboard on the wall, I'd have spotted the problem immediately. Consider
extended health checks. Maybe some CronJobs to exercise things?
