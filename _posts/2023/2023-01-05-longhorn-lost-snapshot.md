---
title: "Longhorn: lost track of the corresponding snapshot info"
date: 2023-01-05T09:29:00.000Z
tags: longhorn
---

Several of my Longhorn volumes became degraded yesterday. They got stuck in a state where they were continually
attempting to rebuild a replica.

In the events, I found at least one message like this (wrapped for, er, readability):

```
51m         Warning   FailedRebuilding   engine/pvc-171325a8-a6e0-4313-88a2-ae67c9c9ba72-e-d0e4dddd
    (combined from similar events):
    Failed rebuilding replica with Address 10.42.0.37:10030: proxyServer=10.42.3.29:8501 destination=10.42.3.29:10000:
    failed to add replica tcp://10.42.0.37:10030 for volume: rpc error: code = Unknown
    desc = failed to sync files [...huge pile of snapshot filenames...]
```

...and a bunch of these...

```
7m35s       Normal    SnapshotUpdate     snapshot/351025d7-5416-4176-a536-7173b748706d                 snapshot becomes ready to use
7m17s       Warning   SnapshotDelete     snapshot/351025d7-5416-4176-a536-7173b748706d                 snapshot is marked as removed
7m17s       Warning   SnapshotUpdate     snapshot/351025d7-5416-4176-a536-7173b748706d                 snapshot becomes not ready to use
6m42s       Warning   SnapshotError      snapshot/351025d7-5416-4176-a536-7173b748706d                 lost track of the corresponding snapshot info inside volume engine
```

I have _no_ idea what's going on, and this is thrashing the disks (and fans) on my cluster nodes. So I just reduced the
replicas from 3 to 2 (to avoid the rebuild) and got on with my day job.

I'm left in an awkward spot, because I'm running a pre-release version of Longhorn (1.4.0 was released only a few days
ago), so that could be the cause of my problems, and I won't get any support (because pre-release version). I _could_
upgrade, but that's not recommended or supported, which means I could lose the volumes entirely.

I haven't practiced fully restoring a volume and mounting it in a pod, so I've no idea how easy that is, which makes me
uneasy.

For now, I guess I'll leave it.

In the next few days, I need to find the time to practice completely restoring a backup -- I can restore the backup to a
volume, and mount that volume on the node (which means I can at least get the files back out of it), but I don't know
how to restore a pod with that volume.

Frankly, though, this is the third or fourth time that Longhorn has gone squirrely on me. I don't know how many more
chances I'm going to give it before I look at alternatives.
