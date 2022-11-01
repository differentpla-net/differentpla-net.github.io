---
title: "longhorn fsck"
date: 2022-10-02T15:26:00Z
tags: longhorn
---

```
  Warning  FailedMount             0s (x5 over 10s)  kubelet                  MountVolume.MountDevice failed for volume "pvc-e014b5f3-fc44-4414-b66e-4638bf9807c7" : rpc error: code = Internal desc = 'fsck' found errors on device /dev/longhorn/pvc-e014b5f3-fc44-4414-b66e-4638bf9807c7 but could not correct them: fsck from util-linux 2.34
/dev/longhorn/pvc-e014b5f3-fc44-4414-b66e-4638bf9807c7 contains a file system with errors, check forced.
/dev/longhorn/pvc-e014b5f3-fc44-4414-b66e-4638bf9807c7: Resize inode not valid.

/dev/longhorn/pvc-e014b5f3-fc44-4414-b66e-4638bf9807c7: UNEXPECTED INCONSISTENCY; RUN fsck MANUALLY.
  (i.e., without -a or -p options)
```

<https://longhorn.io/kb/troubleshooting-volume-filesystem-corruption/>

Edit the deployment, set replicas: 0; I'm using argocd, so I can:
- edit the deploy yaml in the repo and re-sync.
- or just do it manually and (somehow) tell argocd not to fix it.

```
kubectl --namespace docker-registry scale deployment docker-registry --replicas 0
kubectl --namespace docker-registry delete pod docker-registry-54d7c67b5d-kmw2x --force --grace-period=0
```

Mount the disk in another node using the Longhorn UI.

- It will automatically detach after a short while.
- You attach it to a _node_ (i.e. not to a container). I attached it to `rpi401` (the server/control plane node). I did NOT check the 'Maintenance' box. Dunno what that does.

```
$ sudo fsck -y /dev/longhorn/pvc-e014b5f3-fc44-4414-b66e-4638bf9807c7
fsck from util-linux 2.37.2
e2fsck 1.46.5 (30-Dec-2021)
/dev/longhorn/pvc-e014b5f3-fc44-4414-b66e-4638bf9807c7 contains a file system with errors, check forced.
...
```

...wipes the volume. This is not particularly reassuring. OTOH, there's (in theory) nothing particularly valuable on
there.

Welp, we delete the volume in Longhorn (which also deletes the PVC). Then we scale up, which fails, because no PVC. Then
we re-sync in argo, which after a while, brings everything up. Except that the docker registry is now empty.

Time to look at longhorn snapshots and backups. Also: giving more space to the nodes.
