---
title: "k3s on Raspberry Pi: Dynamic Persistent Volumes"
short_title: "Dynamic Persistent Volumes"
date: 2021-12-17T09:06:00Z
layout: series
series: k3s
tags: k3s raspberry-pi
---

We need to give our Docker registry some persistent storage. Currently,
if we restart it, it loses its stored data.

We tried [hostPath]({% post_url 2021/2021-12-16-k3s-docker-registry-hostpath-storage %}), but that didn't work. Let's try persistent volumes.

## Dynamic Persistent Volumes

Create `docker-registry-pvc.yml` as follows:

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

This creates a persistent volume claim for 1Gi of space. Because it's
not associated with a PersistentVolume, it uses [dynamic provisioning](https://kubernetes.io/docs/concepts/storage/persistent-volumes/#dynamic).

```
$ kubectl apply -f docker-registry-pvc.yml
persistentvolumeclaim/docker-registry-pvc created
```

Edit the `docker-registry.yml` file [from before]({% post_url 2021/2021-12-10-k3s-docker-registry %}).

It should look like this:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: docker-registry
  namespace: docker-registry
  labels:
    app: docker-registry
spec:
  replicas: 1
  selector:
    matchLabels:
      app: docker-registry
  template:
    metadata:
      labels:
        app: docker-registry
        name: docker-registry
    spec:
      containers:
      - name: registry
        image: registry:2
        ports:
        - containerPort: 5000
        volumeMounts:
        - name: docker-registry-vol
          mountPath: /var/lib/registry
      volumes:
      - name: docker-registry-vol
        persistentVolumeClaim:
          claimName: docker-registry-pvc
```

We've added the `volumes` and `volumeMounts` sections. These define a
volume (using `persistentVolumeClaim` referring to the PVC we just
created), and a volume mount (which mounts that volume inside the
container).

<div class="callout callout-info" markdown="span">
Aside: if you don't specify the volume type it uses `emptyDir`. This
is probably not what you want.

See [the docs](https://kubernetes.io/docs/concepts/storage/volumes/#emptydir)
for details.
</div>

Apply that:

```
$ kubectl apply -f docker-registry.yml
deployment.apps/docker-registry configured
```

After a short while, your new pod should be running using the persistent storage.

```
$ kubectl --namespace docker-registry get pods
NAME                              READY   STATUS    RESTARTS   AGE
docker-registry-887c57b6b-7lvx9   1/1     Running   0          12m
```

```
$ kubectl --namespace docker-registry describe pod docker-registry-887c57b6b-7lvx9
...
Node:         rpi404/192.168.28.182
...
Containers:
  registry:
    ...
    Mounts:
      /var/lib/registry from docker-registry-vol (rw)
...
Volumes:
  docker-registry-vol:
    Type:       PersistentVolumeClaim (a reference to a PersistentVolumeClaim in the same namespace)
    ClaimName:  docker-registry-pvc
    ReadOnly:   false
...
```

```
$ kubectl --namespace docker-registry get pvc
NAME                  STATUS   VOLUME                                     CAPACITY   ACCESS MODES   STORAGECLASS   AGE
docker-registry-pvc   Bound    pvc-46cdbfbe-7db7-4079-a6ce-625740f658d5   1Gi        RWO            local-path     64m

$ kubectl --namespace docker-registry get pv
NAME                                       CAPACITY   ACCESS MODES   RECLAIM POLICY   STATUS   CLAIM                                 STORAGECLASS   REASON   AGE
pvc-46cdbfbe-7db7-4079-a6ce-625740f658d5   1Gi        RWO            Delete           Bound    docker-registry/docker-registry-pvc   local-path              62m
```

## Is it persistent?

As [before]({% post_url 2021/2021-12-16-k3s-docker-registry-hostpath-storage %}), we'll push a "hello-world" image to the registry.

```
$ docker pull hello-world
$ docker tag hello-world 10.43.236.176:5000/hello-world
$ docker push 10.43.236.176:5000/hello-world
$ docker pull 10.43.236.176:5000/hello-world
Using default tag: latest
latest: Pulling from hello-world
Digest: sha256:f130bd2d67e6e9280ac6d0a6c83857bfaf70234e8ef4236876eccfbd30973b1c
Status: Image is up to date for 10.43.236.176:5000/hello-world:latest
10.43.236.176:5000/hello-world:latest
```

Then we delete the pod:

```
$ kubectl --namespace docker-registry delete pod docker-registry-887c57b6b-7lvx9
pod "docker-registry-887c57b6b-7lvx9" deleted
```

And wait for the replacement pod to start:

```
$ kubectl --namespace docker-registry get pods
NAME                              READY   STATUS    RESTARTS   AGE
docker-registry-887c57b6b-8prq2   1/1     Running   0          42s
```

Then we pull the image again:

```
$ docker pull 10.43.236.176:5000/hello-world
Using default tag: latest
latest: Pulling from hello-world
Digest: sha256:f130bd2d67e6e9280ac6d0a6c83857bfaf70234e8ef4236876eccfbd30973b1c
Status: Image is up to date for 10.43.236.176:5000/hello-world:latest
10.43.236.176:5000/hello-world:latest
```

It worked. We now have persistent storage for our docker registry. There
are downsides with this approach; I'll talk about them later.

## Where are my files?

```
$ kubectl --namespace docker-registry describe pvc docker-registry-pvc
...
Volume:        pvc-46cdbfbe-7db7-4079-a6ce-625740f658d5
Labels:        <none>
Annotations:   pv.kubernetes.io/bind-completed: yes
               pv.kubernetes.io/bound-by-controller: yes
               volume.beta.kubernetes.io/storage-provisioner: rancher.io/local-path
               volume.kubernetes.io/selected-node: rpi404
...
```

```
$ kubectl --namespace docker-registry describe pv pvc-46cdbfbe-7db7-4079-a6ce-625740f658d5
...
Source:
    Type:          HostPath (bare host directory volume)
    Path:          /var/lib/rancher/k3s/storage/pvc-46cdbfbe-7db7-4079-a6ce-625740f658d5_docker-registry_docker-registry-pvc
    HostPathType:  DirectoryOrCreate
...
```

Apparently, they're in `/var/lib/rancher/k3s/storage/pvc-bunch-of-numbers` on `rpi404`. Let's check:

```
% ssh rpi404
$ sudo bash
# cd /var/lib/rancher/k3s/storage/pvc-46cdbfbe-7db7-4079-a6ce-625740f658d5_docker-registry_docker-registry-pvc

# ls -F
docker/

# find . -type f
./docker/registry/v2/repositories/hello-world/_manifests/tags/latest/current/link
...
```

## local-path storage has node affinity

But if my files are on the local filesystem on a particular node, what
happens if the pod moves?

Let's force the pod to move by disabling the current node:

```
$ kubectl --namespace docker-registry get pods -o wide
NAME                              READY   STATUS    RESTARTS   AGE     IP           NODE     NOMINATED NODE   READINESS GATES
docker-registry-887c57b6b-8prq2   1/1     Running   0          7m20s   10.42.3.40   rpi404   <none>           <none>

$ kubectl cordon rpi404
node/rpi404 cordoned
```

Then we delete the pod:

```
$ kubectl --namespace docker-registry delete pod docker-registry-887c57b6b-8prq2

$ kubectl --namespace docker-registry get pods -o wide
NAME                              READY   STATUS    RESTARTS   AGE   IP       NODE     NOMINATED NODE   READINESS GATES
docker-registry-887c57b6b-j9276   0/1     Pending   0          26s   <none>   <none>   <none>           <none>
```

The node is now stuck in `Pending`. This is because there are no
suitable nodes for it to run on:

```
$ kubectl --namespace docker-registry describe pod docker-registry-887c57b6b-j9276
...
Events:
  Type     Reason            Age   From               Message
  ----     ------            ----  ----               -------
  Warning  FailedScheduling  78s   default-scheduler  0/5 nodes are available: 1 node(s) were unschedulable, 4 node(s) had volume node affinity conflict.
```

Because we're using the `local-path` storage class (the default for
k3s), the PVC has node affinity:

```
$ kubectl --namespace docker-registry describe pv pvc-46cdbfbe-7db7-4079-a6ce-625740f658d5
...
Node Affinity:
  Required Terms:
    Term 0:        kubernetes.io/hostname in [rpi404]
...
```

If we re-enable the node, the pod starts:

```
$ kubectl uncordon rpi404
$ kubectl --namespace docker-registry get pods -o wide
NAME                              READY   STATUS    RESTARTS   AGE     IP           NODE     NOMINATED NODE   READINESS GATES
docker-registry-887c57b6b-j9276   1/1     Running   0          9m36s   10.42.3.41   rpi404   <none>           <none>
```

Awesome, we now have persistent storage for our docker registry. Any day now, we'll be able to start using it to serve images.

But first, we've got to deal with the following:

- Maybe we don't like the default location of `/var/lib/rancher/k3s/storage`.
  - We need to talk about static provisioning.
- Because it's got node affinity, if that node's dead, we've got problems.
  - We can use shared storage, such as [iSCSI](% post_url 2021/2021-12-09-k3s-raspi-iscsi %}) for that.
- We're still using the dotted IP name. We don't have TLS certificates.
  - Both of these require that we get into naming and DNS.
- We don't have any auth.
  - This one's (relatively) trivial after all we've gone through so far.
  - In fact, I should probably have dealt with it earlier.
