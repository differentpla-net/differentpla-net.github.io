---
title: "k3s on Raspberry Pi: Static Persistent Volumes"
short_title: "Static Persistent Volumes"
date: 2021-12-17T15:09:00Z
layout: series
series: k3s
tags: k3s raspberry-pi
---

In [the previous post]({% post_url 2021/2021-12-17-k3s-dynamic-pv %}), we succeeded in giving our docker registry some
persistent storage. However, it used (the default) dynamic provisioning, which means we don't have as much control over
_where_ the storage is provisioned.

## The local-path storage class

Previously, we created a PersistentVolumeClaim:

```yaml
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: docker-registry-pvc
  namespace: docker-registry
spec:
  accessModes:
  - ReadWriteOnce
  resources:
    requests:
      storage: 1Gi
```

And then we attached it to our container:

```yaml
...
      volumes:
      - name: docker-registry-vol
        persistentVolumeClaim:
          claimName: docker-registry-pvc
```

Because we didn't specify anything in the claim, the default storage class is used.

For k3s, this is `local-path`:

```
$ kubectl get storageclass
NAME                   PROVISIONER             RECLAIMPOLICY   VOLUMEBINDINGMODE      ALLOWVOLUMEEXPANSION   AGE
local-path (default)   rancher.io/local-path   Delete          WaitForFirstConsumer   false                  161d
```

By default, this provisions storage in the `/var/lib/rancher/k3s/storage` directory on the local filesystem of each
node:

```
$ kubectl --namespace kube-system get cm local-path-config -o yaml
apiVersion: v1
data:
  config.json: |-
    {
      "nodePathMap":[
      {
        "node":"DEFAULT_PATH_FOR_NON_LISTED_NODES",
        "paths":["/var/lib/rancher/k3s/storage"]
      }
      ]
    }
...
```

If we wanted to provision storage on a different filesystem, we could edit the ConfigMap appropriately.

For example, some of our nodes might have extra storage, or we might want to use an NFS server for shared storage.

## Static Provisioning

Another way to do it would be to use static provisioning, where we explicitly create a PersistentVolume, and then
specify it in our PersistentVolumeClaim.

One example I found -- at [Deploy Your Private Docker Registry as a Pod in Kubernetes](https://medium.com/swlh/deploy-your-private-docker-registry-as-a-pod-in-kubernetes-f6a489bf0180),
gives us the following:

{% include alerts/warning.html content="**Warning:** This _seems_ to work, but it fails to associate the claim with the volume." %}

```yaml
apiVersion: v1
kind: PersistentVolume
metadata:
  name: docker-repo-pv
spec:
  capacity:
    storage: 1Gi
  accessModes:
  - ReadWriteOnce
  hostPath:
    path: /tmp/repository
---
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: docker-repo
spec:
  accessModes:
  - ReadWriteOnce
  resources:
    requests:
      storage: 1Gi
```

If you apply the above, you'll end up with a dynamic PVC (and PV) and an (unused) static PV:

```
$ kubectl --namespace docker-registry get pvc
NAME                  STATUS   VOLUME                                     CAPACITY   ACCESS MODES   STORAGECLASS   AGE
docker-registry-pvc   Bound    pvc-938faff3-c285-4105-83bc-221bb93a6603   1Gi        RWO            local-path     8m32s

$ kubectl --namespace docker-registry get pv
NAME                                       CAPACITY   ACCESS MODES   RECLAIM POLICY   STATUS      CLAIM                                 STORAGECLASS   REASON   AGE
docker-registry-pv                         1Gi        RWO            Retain           Available                                                                 18m
pvc-938faff3-c285-4105-83bc-221bb93a6603   1Gi        RWO            Delete           Bound       docker-registry/docker-registry-pvc   local-path              3m42s
```

You can see that we have one PVC listed, but two PV's. One of the PV's is marked "Available", showing that it's unused.

For it to be used, there has to be some kind of association between the claim and the volume.

One way to do that is given in the [Configure a Pod to Use a PersistentVolume for Storage][create-a-persistentvolume]
walkthrough, which uses an explicit `storageClassName: manual` on the volume, which is used to bind the claim.

[create-a-persistentvolume]: https://kubernetes.io/docs/tasks/configure-pod-container/configure-persistent-volume-storage/#create-a-persistentvolume

It looks like you can invent storage classes to describe whether you're providing (for example) slow-but-reliable vs.
fast-but-unreliable storage. Then you'd provision a number of PV instances (with whatever backing store is relevant).
Then each claim can use the appropriate storage class name to request the type of storage (reliable or fast) it needs.

Presumably there's a storage scheduler in there somewhere that matches these up in some "optimal" way.

## Labels and selectors

Another way to do this is to use labels on the PV, and a matching selector on the PVC.

Let's experiment with that by converting our [iSCSI]({% post_url 2021/2021-12-09-k3s-raspi-iscsi %}) deployment to use a
PV and PVC.

The simplest way to do this seems to be to use an `app: testing` label in the volume and then use it as the selector in
the claim:

{% include alerts/warning.html content="**Warning:** This leaves the claim Pending; read on for why." %}

<div class="callout callout-info" markdown="span">
I later discovered that you can reserve (pre-bind) volume claims to volumes.
See [Reserving a PersistentVolume](https://kubernetes.io/docs/concepts/storage/persistent-volumes/#reserving-a-persistentvolume) in the K8s documentation.
</div>

```yaml
apiVersion: v1
kind: PersistentVolume
metadata:
  name: testing-vol-pv
  labels:
    app: testing        # <--
spec:
  capacity:
    storage: 1Gi
  accessModes:
  - ReadWriteOnce
  iscsi:
    targetPortal: 192.168.28.124:3260
    iqn: iqn.2000-01.com.synology:ds211.testing.25e6c0dc53
    lun: 1
    readOnly: false
---
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: testing-vol-pvc
spec:
  accessModes:
  - ReadWriteOnce
  resources:
    requests:
      storage: 1Gi
  selector:
    matchLabels:
      app: testing      # <--
```

If we apply that:

```
$ kubectl apply -f testing-ubuntu-pv.yml
persistentvolume/testing-vol-pv configured
persistentvolumeclaim/testing-vol-pvc created
```

```
$ kubectl get pv
NAME                                       CAPACITY   ACCESS MODES   RECLAIM POLICY   STATUS      CLAIM                                 STORAGECLASS   REASON   AGE
testing-vol-pv                             1Gi        RWO            Retain           Available                                                                 101s

$ kubectl get pvc
NAME              STATUS    VOLUME   CAPACITY   ACCESS MODES   STORAGECLASS   AGE
testing-vol-pvc   Pending                                      local-path     21s
```

If we look into why it's still `Pending`:

```
$ kubectl describe pvc testing-vol-pvc
...
Events:
  Type    Reason                Age                From                         Message
  ----    ------                ----               ----                         -------
  Normal  WaitForFirstConsumer  13s (x3 over 41s)  persistentvolume-controller  waiting for first consumer to be created before binding
```

OK then. Let's create our first consumer. We'll update our deployment:

```yaml
...
      volumes:
      - name: testing-vol
        persistentVolumeClaim:
          claimName: testing-vol-pvc
```

...and fire it up:

```
$ kubectl apply -f testing-ubuntu.yml
deployment.apps/testing created
```

That doesn't work. It remains `Pending`. Let's investigate:

```
$ kubectl get events
LAST SEEN   TYPE      REASON                 OBJECT                                  MESSAGE
17s         Normal    ExternalProvisioning   persistentvolumeclaim/testing-vol-pvc   waiting for a volume to be created, either by external provisioner "rancher.io/local-path" or manually created by system administrator
14s         Normal    Provisioning           persistentvolumeclaim/testing-vol-pvc   External provisioner is provisioning volume for claim "default/testing-vol-pvc"
14s         Warning   ProvisioningFailed     persistentvolumeclaim/testing-vol-pvc   failed to provision volume with StorageClass "local-path": claim.Spec.Selector is not supported
```

Ah. Seems we need to use a different storage class. Let's try that.

However, we first need to delete the PV and PVC, because we're not allowed to change the `spec` after they've been
provisioned.

## Deleting a PersistentVolume and PersistentVolumeClaim

```
$ kubectl delete pv testing-vol-pv
persistentvolume "testing-vol-pv" deleted

$ kubectl delete pvc testing-vol-pvc --grace-period=0 --force
warning: Immediate deletion does not wait for confirmation that the running resource has been terminated. The resource may continue to run on the cluster indefinitely.
persistentvolumeclaim "testing-vol-pvc" force deleted
```

## Adding a storageClassName

Update the `testing-ubuntu-pv.yml` file:

```yaml
apiVersion: v1
kind: PersistentVolume
metadata:
  name: testing-vol-pv
  labels:
    app: testing
spec:
  storageClassName: iscsi   # <--
  capacity:
    storage: 1Gi
  accessModes:
  - ReadWriteOnce
  iscsi:
    targetPortal: 192.168.28.124:3260
    iqn: iqn.2000-01.com.synology:ds211.testing.25e6c0dc53
    lun: 1
    readOnly: false
---
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: testing-vol-pvc
spec:
  storageClassName: iscsi   # <--
  accessModes:
  - ReadWriteOnce
  resources:
    requests:
      storage: 1Gi
  selector:
    matchLabels:
      app: testing
```

Then reapply it (and the deployment)

```
$ kubectl apply -f testing-ubuntu-pv.yml
persistentvolume/testing-vol-pv configured
persistentvolumeclaim/testing-vol-pvc created

$ kubectl apply -f testing-ubuntu.yml
```

It seems to work:

```
$ kubectl get pods -o wide
NAME                       READY   STATUS    RESTARTS   AGE   IP           NODE     NOMINATED NODE   READINESS GATES
testing-5cb897dd66-sk5zs   1/1     Running   0          83s   10.42.2.41   rpi405   <none>           <none>
```

## Is it persistent? Across nodes?

We're on the home straight at this point.

```
$ kubectl exec --stdin --tty testing-5d4458cc68-jffcx -- /bin/bash
# ls /var/lib/testing
kilroy-was-here
```

Cool, so we didn't lose anything while we were messing around.

```
# touch /var/lib/testing/kilroy-was-still-here
```

Let's force the pod to move:

```
$ kubectl cordon rpi405
$ kubectl delete pod testing-5cb897dd66-sk5zs
$ kubectl get pods -o wide
NAME                       READY   STATUS    RESTARTS   AGE    IP           NODE     NOMINATED NODE   READINESS GATES
testing-5cb897dd66-xmjt5   1/1     Running   0          100s   10.42.3.42   rpi404   <none>           <none>
```

New pod, different node.

```
$ kubectl exec --stdin --tty testing-5cb897dd66-xmjt5 -- /bin/bash
# ls /var/lib/testing/
kilroy-was-here  kilroy-was-still-here
```

Same content. We're done.

## What was the point?

That seems like a lot of effort to get from where we were -- a single pod with persistent storage on an iSCSI target --
to where we are -- a single pod with persistent storage on an iSCSI target, except specified with a volume claim.

Honestly, at this point, I don't have a definitive reason for that.

I suspect that if I'd provisioned a number of iSCSI targets and created a volume for each, then Kubernetes (or k3s)
could have used them to fulfill a corresponding number of volume claims without needing to worry specifically about what
went where, as long as the storage class (`iscsi`) matched up.

In a word: abstraction.

We're moving the volume details one step back from the pod, allowing us more flexibility in how we provision it.

Maybe it would be clearer if the PV and PVC weren't in the same YAML file.

The claim should go in the deployment YAML. It's saying "this deployment needs a volume; it should have this much capacity,
and it should be in this storage class". It's up to the scheduler to match that up with an available volume, or to provision
one dynamically.

Ideally, we'd invent a storage provisioner that used the Synology NAS API (does it have one?) to carve out targets and
LUNs on demand. That'd be cool, but it's overkill for my home test lab.

We've also not really solved our "node is unavailable, we can't start the pod" problem either. We've just moved _that_
one step back as well. Now we have a "NAS is unavailable, we can't start the pod" problem.

But, in theory, a reasonable NAS system (even a 10-year old one) is more reliable than a bunch of Raspberry Pi 4s
precariously balanced on the edge of my desk.

<div class="callout callout-warning" markdown="span">
Maybe I spoke [too soon]({% post_url 2021/2021-12-18-k3s-raspi-docker-push-fails %})?
</div>
