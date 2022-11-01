---
title: "Volume stuck in Terminating"
date: 2022-10-17T14:56:00Z
tags: k3s kubernetes
layout: series
series: k3s
---

Longhorn volume degraded. Filesystem read-only. Need to figure out why this keeps happening. Couldn't figure out how to recover it. Decided to re-create the pod and volume.

First, we scale down the deployment, so that there are no longer any users of the volume:

kubectl --namespace monitoring-system scale deployment vmsingle-vm-database --replicas 0

Then we delete the pod:

kubectl --namespace monitoring-system delete pod vmsingle-vm-database-5464c7ddf-2nqgq

Then we delete the PVC and PV:

kubectl --namespace monitoring-system delete pvc vmsingle-vm-database --force --grace-period=0
kubectl --namespace monitoring-system delete pv pvc-ef3c7b30-e56e-494a-b70e-6ea1e954d134 --force --grace-period=0

But at this point, the PV and PVC are stuck in Terminating:

k3s/home [master] % kubectl --namespace monitoring-system get pvc
NAME                   STATUS        VOLUME                                     CAPACITY   ACCESS MODES   STORAGECLASS   AGE
vmsingle-vm-database   Terminating   pvc-ef3c7b30-e56e-494a-b70e-6ea1e954d134   1Gi        RWO            longhorn       27h
k3s/home [master] % kubectl --namespace monitoring-system get pv
NAME                                       CAPACITY   ACCESS MODES   RECLAIM POLICY   STATUS        CLAIM                                    STORAGECLASS   REASON   AGE
pvc-9a617686-d0f2-4c91-adb4-5daca80e72e9   10Gi       RWO            Delete           Bound         gitea/data-gitea-0                       longhorn                260d
pvc-35446ed4-1284-47a1-a697-9f6d6a73b3be   5Gi        RWO            Delete           Bound         gitea/pgdata-gitea-postgres-0            longhorn                260d
pvc-e00801ee-0029-4c78-889c-7fb2da300aab   1Gi        RWO            Delete           Bound         livebook/livebook-data-pvc               longhorn                235d
pvc-0c20c7e9-ad00-4a04-87d4-b6c1f4d26b42   1Gi        RWO            Delete           Bound         docker-registry/docker-registry-pvc      longhorn                14d
pvc-8a6e7787-35fb-4096-9799-64e1cecf9998   1Gi        RWO            Delete           Bound         grafana/grafana-pvc                      longhorn                29h
pvc-ef3c7b30-e56e-494a-b70e-6ea1e954d134   1Gi        RWO            Delete           Terminating   monitoring-system/vmsingle-vm-database   longhorn                27h

There's still a finalizer attached to the PVC:

kubectl --namespace monitoring-system get pvc vmsingle-vm-database -o yaml
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  annotations:
    pv.kubernetes.io/bind-completed: "yes"
    pv.kubernetes.io/bound-by-controller: "yes"
    volume.beta.kubernetes.io/storage-provisioner: driver.longhorn.io
    volume.kubernetes.io/storage-provisioner: driver.longhorn.io
  creationTimestamp: "2022-10-16T11:19:48Z"
  deletionGracePeriodSeconds: 0
  deletionTimestamp: "2022-10-17T14:14:09Z"
  finalizers:
  - apps.victoriametrics.com/finalizer
  labels:
    app.kubernetes.io/component: monitoring
    app.kubernetes.io/instance: vm-database
    app.kubernetes.io/name: vmsingle
    managed-by: vm-operator
  name: vmsingle-vm-database
  namespace: monitoring-system
  resourceVersion: "15882331"
  uid: ef3c7b30-e56e-494a-b70e-6ea1e954d134
spec:

Remove the finalizer:

% kubectl --namespace monitoring-system patch pvc vmsingle-vm-database -p '{"metadata":{"finalizers":null}}'
persistentvolumeclaim/vmsingle-vm-database patched

At this point, the PV is release; need to delete it in the Longhorn UI:

 % kubectl --namespace monitoring-system get pv
NAME                                       CAPACITY   ACCESS MODES   RECLAIM POLICY   STATUS     CLAIM                                    STORAGECLASS   REASON   AGE
pvc-9a617686-d0f2-4c91-adb4-5daca80e72e9   10Gi       RWO            Delete           Bound      gitea/data-gitea-0                       longhorn                268d
pvc-35446ed4-1284-47a1-a697-9f6d6a73b3be   5Gi        RWO            Delete           Bound      gitea/pgdata-gitea-postgres-0            longhorn                267d
pvc-e00801ee-0029-4c78-889c-7fb2da300aab   1Gi        RWO            Delete           Bound      livebook/livebook-data-pvc               longhorn                243d
pvc-0c20c7e9-ad00-4a04-87d4-b6c1f4d26b42   1Gi        RWO            Delete           Bound      docker-registry/docker-registry-pvc      longhorn                22d
pvc-8a6e7787-35fb-4096-9799-64e1cecf9998   1Gi        RWO            Delete           Bound      grafana/grafana-pvc                      longhorn                8d
pvc-980083a2-94c6-4a5b-a0e1-edd8818ee340   1Gi        RWO            Delete           Released   monitoring-system/vmsingle-vm-database   longhorn                7d17h

It goes terminating:

~ % kubectl --namespace monitoring-system get pv
NAME                                       CAPACITY   ACCESS MODES   RECLAIM POLICY   STATUS        CLAIM                                    STORAGECLASS   REASON   AGE
pvc-9a617686-d0f2-4c91-adb4-5daca80e72e9   10Gi       RWO            Delete           Bound         gitea/data-gitea-0                       longhorn                268d
pvc-35446ed4-1284-47a1-a697-9f6d6a73b3be   5Gi        RWO            Delete           Bound         gitea/pgdata-gitea-postgres-0            longhorn                267d
pvc-e00801ee-0029-4c78-889c-7fb2da300aab   1Gi        RWO            Delete           Bound         livebook/livebook-data-pvc               longhorn                243d
pvc-0c20c7e9-ad00-4a04-87d4-b6c1f4d26b42   1Gi        RWO            Delete           Bound         docker-registry/docker-registry-pvc      longhorn                22d
pvc-8a6e7787-35fb-4096-9799-64e1cecf9998   1Gi        RWO            Delete           Bound         grafana/grafana-pvc                      longhorn                8d
pvc-980083a2-94c6-4a5b-a0e1-edd8818ee340   1Gi        RWO            Delete           Terminating   monitoring-system/vmsingle-vm-database   longhorn                7d17h

PVC is gone; PV is gone:

k3s/home [master] % kubectl --namespace monitoring-system get pvc vmsingle-vm-database -o yaml
Error from server (NotFound): persistentvolumeclaims "vmsingle-vm-database" not found
k3s/home [master] % kubectl --namespace monitoring-system get pv
NAME                                       CAPACITY   ACCESS MODES   RECLAIM POLICY   STATUS   CLAIM                                 STORAGECLASS   REASON   AGE
pvc-9a617686-d0f2-4c91-adb4-5daca80e72e9   10Gi       RWO            Delete           Bound    gitea/data-gitea-0                    longhorn                260d
pvc-35446ed4-1284-47a1-a697-9f6d6a73b3be   5Gi        RWO            Delete           Bound    gitea/pgdata-gitea-postgres-0         longhorn                260d
pvc-e00801ee-0029-4c78-889c-7fb2da300aab   1Gi        RWO            Delete           Bound    livebook/livebook-data-pvc            longhorn                235d
pvc-0c20c7e9-ad00-4a04-87d4-b6c1f4d26b42   1Gi        RWO            Delete           Bound    docker-registry/docker-registry-pvc   longhorn                14d
pvc-8a6e7787-35fb-4096-9799-64e1cecf9998   1Gi        RWO            Delete           Bound    grafana/grafana-pvc                   longhorn                29h

Scale up:

% kubectl --namespace monitoring-system scale deployment vmsingle-vm-database --replicas 1
deployment.apps/vmsingle-vm-database scaled

Wait.

Everything's happy again. Except that the volume was wiped. Probably not a big deal in this case.

Lost all of the metrics before 09.15. Which, given that I only rebuilt it at 16.00 ish, is impressive. Presumably the VM agent keeps some data if the DB is down.

Something kept re-creating the pod, even though the replicas=0; what?
