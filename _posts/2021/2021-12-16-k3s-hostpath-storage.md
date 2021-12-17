---
title: "k3s on Raspberry Pi: Persistent Storage using hostPath"
short_title: "Persistent Storage using hostPath"
date: 2021-12-16T18:20:00Z
layout: series
series: k3s
tags: k3s raspberry-pi
---

We need to give our Docker registry some persistent storage. Currently,
if we restart it, it loses its stored data.

## hostPath

We can specify
[hostPath](https://kubernetes.io/docs/concepts/storage/volumes/#hostpath)
directly in the container specification. That allows us to mount a file
or directory from the host.

{% capture warning %}
**Warning:** This doesn't work. There are ways to make it work [at the bottom](#how-to-fix-it). For something that does work, see [k3s on Raspberry Pi: Dynamic Persistent Volumes]({% post_url 2021/2021-12-17-k3s-dynamic-pv %}).
{% endcapture %}
{% include alerts/warning.html content=warning %}

To do that, edit the `docker-registry.yml` file [from before]({% post_url 2021/2021-12-10-k3s-docker-registry %}).

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
      volumes:
      - name: docker-registry-vol
        hostPath:
          path: /tmp/docker-registry
      containers:
      - name: registry
        image: registry:2
        ports:
        - containerPort: 5000
        volumeMounts:
        - name: docker-registry-vol
          mountPath: /var/lib/registry
```

We've added the `volumes` and `volumeMounts` sections. These define a
volume (using `hostPath`, so that it's stored on the node's filesystem),
and a volume mount (which mounts that volume inside the container).

Apply it:

```
$ kubectl apply -f docker-registry.yml
deployment.apps/docker-registry configured
```

Because we're using a ClusterIP service, the IP address
[doesn't change](https://cloud.google.com/kubernetes-engine/docs/concepts/service#services_of_type_clusterip),
which is convenient.

But, you'll note, the previous ephemeral storage was discarded:

```
$ docker pull 10.43.236.176:5000/hello-world
Using default tag: latest
Error response from daemon: manifest for 10.43.236.176:5000/hello-world:latest not found: manifest unknown: manifest unknown
```

Let's see if what we've got now is better. We'll test it by pushing a container...

```
$ docker pull hello-world
$ docker tag hello-world 10.43.236.176:5000/hello-world
$ docker push 10.43.236.176:5000/hello-world
$ docker pull 10.43.236.176:5000/hello-world
```

...deleting the pod...

```
$ kubectl --namespace docker-registry delete pod docker-registry-87bc44c4d-2w4zr
pod "docker-registry-87bc44c4d-2w4zr" deleted
```

...waiting for the replacement pod to start...

```
$ kubectl --namespace docker-registry get pods
NAME                              READY   STATUS    RESTARTS   AGE
docker-registry-87bc44c4d-stns9   1/1     Running   0          42s
```

...and then attempting to pull the image again:


```
$ docker pull 10.43.236.176:5000/hello-world
Using default tag: latest
Error response from daemon: manifest for 10.43.236.176:5000/hello-world:latest not found: manifest unknown: manifest unknown
```

Oh, it ... didn't work.

## What happened?

The problem is that the pod moved to a different node (comparing the
before and after `describe pod` output...). So, while the pod has access
to local storage on the node, it's not the same storage, because it's
_not the same node_.

## Where are my files?

If we used `hostPath`, these files have got to have gone somewhere,
right? Our pod initially ran on `rpi403`, and our `hostPath` specified
`path: /tmp/docker-registry`, so let's go looking.

```
% ssh rpi403
$ ls -F /tmp
docker-registry/  ssh-flurble/  systemd-private-string-of-numbers/
etc
$ cd /tmp/docker-registry
$ find . -type f
./docker/registry/v2/repositories/hello-world/_manifests/tags/latest/current/link
...
```

Yep. Those are our docker images.

If we look in the same place on the new node, we'll find the
`/tmp/docker-registry` directory, but it's empty.

## How to fix it?

We could fix this in a few different ways:
- Use node affinity to always start the pod on the same node.
- Use some kind of shared storage, such as NFS, so that all nodes have the same view of the volume.

Or we could use [persistent volumes]({% post_url 2021/2021-12-17-k3s-dynamic-pv %}).

## References

- [How to Setup Private Docker Registry in Kubernetes (k8s)](https://www.linuxtechi.com/setup-private-docker-registry-kubernetes/) -- using `hostPath` in the container.
  - The paths refer to NFS mounts, which is one way to get shared persistent storage.
