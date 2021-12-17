---
title: "k3s on Raspberry Pi: Using an Insecure Docker Registry"
short_title: "Using an Insecure Docker Registry"
date: 2021-12-16T16:16:00Z
layout: series
series: k3s
tags: k3s raspberry-pi
---

Let's see if we can push an image to our new Docker Registry.

## Installing Docker CLI

First we need to install the Docker CLI.

Because this is Raspbian, we need to use the "convenience" script that Docker provides.
See [Install using the convenience script](https://docs.docker.com/engine/install/debian/#install-using-the-convenience-script).

Like it says:

> Tip: preview script steps before running

```bash
curl -fsSL https://get.docker.com -o get-docker.sh
DRY_RUN=1 sh ./get-docker.sh
```

Happy? Then we can run it:

```bash
sudo sh get-docker.sh
```

## Pull "hello-world"

We're going to need _something_ to push, so we'll just use the "hello-world" image:

```bash
$ sudo docker pull hello-world
Using default tag: latest
latest: Pulling from library/hello-world
9b157615502d: Pull complete
Digest: sha256:cc15c5b292d8525effc0f89cb299f1804f3a725c8d05e158653a563f15e4f685
Status: Downloaded newer image for hello-world:latest
docker.io/library/hello-world:latest
```

## docker permissions

That `sudo docker` stuff is going to get old quickly. Let's sort out permissions:

```
$ sudo usermod -aG docker pi
$ exit   # and log back in
```

Note that if you're using Ubuntu, systemd will re-use your
session when you log back in. You basically need to reboot to sort it
out.

Fortunately, we're not on Ubuntu; we're accessing the node over SSH, so
a simple `Ctrl+D`, `Up`, `Enter` gets us logged back in.

## Pushing an image

```bash
$ docker tag hello-world 10.43.236.176:5000/hello-world

$ docker push 10.43.236.176:5000/hello-world
Using default tag: latest
The push refers to repository [10.43.236.176:5000/hello-world]
Get "https://10.43.236.176:5000/v2/": http: server gave HTTP response to HTTPS client
```

Ah. That's a problem. Can we tell docker to not use HTTPS for now?

## Configuring docker to use an insecure registry

Yes, you can. See [this Stack Overflow question](https://stackoverflow.com/q/49674004/).

Create (or edit) `/etc/docker/daemon.json`, as follows:

```json
{"insecure-registries": ["10.43.236.176:5000"]}
```

Then restart the docker daemon:

```bash
sudo systemctl restart docker
```

```bash
$ sudo docker push 10.43.236.176:5000/hello-world
Using default tag: latest
The push refers to repository [10.43.236.176:5000/hello-world]
a380e59f1cab: Pushed
latest: digest: sha256:f130bd2d67e6e9280ac6d0a6c83857bfaf70234e8ef4236876eccfbd30973b1c size: 525
```

```bash
$ sudo docker run 10.43.236.176:5000/hello-world

Hello from Docker!
...
```

## Using an image

At this point, we should be able to run a container with that image, so let's create `hello-world.yml`:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: hello-world
  namespace: docker-registry
  labels:
    app: hello-world
spec:
  replicas: 1
  selector:
    matchLabels:
      app: hello-world
  template:
    metadata:
      labels:
        app: hello-world
        name: hello-world
    spec:
      containers:
      - name: hello-world
        image: 10.43.236.176:5000/hello-world
```

```
$ kubectl apply -f hello-world.yml
deployment.apps/hello-world created
```

```
$ kubectl --namespace docker-registry get all
NAME                                   READY   STATUS         RESTARTS   AGE
pod/docker-registry-684dc65c99-pckxl   1/1     Running        0          137m
pod/hello-world-5449677b5b-96crm       0/1     ErrImagePull   0          6s

NAME                      TYPE        CLUSTER-IP      EXTERNAL-IP   PORT(S)    AGE
service/docker-registry   ClusterIP   10.43.236.176   <none>        5000/TCP   106m

NAME                              READY   UP-TO-DATE   AVAILABLE   AGE
deployment.apps/docker-registry   1/1     1            1           137m
deployment.apps/hello-world       0/1     1            0           7s

NAME                                         DESIRED   CURRENT   READY   AGE
replicaset.apps/docker-registry-684dc65c99   1         1         1       137m
replicaset.apps/hello-world-5449677b5b       1         1         0       6s
```

Hmmm. `ErrImagePull` doesn't look good. Let's investigate.

```bash
$ kubectl --namespace docker-registry describe pod hello-world-5449677b5b-96crm
Name:         hello-world-5449677b5b-96crm
Namespace:    docker-registry
...
Events:
  Type     Reason     Age                From               Message
  ----     ------     ----               ----               -------
  Normal   Scheduled  94s                default-scheduler  Successfully assigned docker-registry/hello-world-5449677b5b-96crm to rpi403
  Normal   BackOff    26s (x4 over 93s)  kubelet            Back-off pulling image "10.43.236.176:5000/hello-world"
  Warning  Failed     26s (x4 over 93s)  kubelet            Error: ImagePullBackOff
  Normal   Pulling    12s (x4 over 93s)  kubelet            Pulling image "10.43.236.176:5000/hello-world"
  Warning  Failed     12s (x4 over 93s)  kubelet            Failed to pull image "10.43.236.176:5000/hello-world": rpc error: code = Unknown desc = failed to pull and unpack image "10.43.236.176:5000/hello-world:latest": failed to resolve reference "10.43.236.176:5000/hello-world:latest": failed to do request: Head "https://10.43.236.176:5000/v2/hello-world/manifests/latest": http: server gave HTTP response to HTTPS client
  Warning  Failed     12s (x4 over 93s)  kubelet            Error: ErrImagePull
```

Yeah, we get the same `server gave HTTP response to HTTPS client` error
as before.

Let's clean up:

```bash
kubectl --namespace docker-registry delete deployment hello-world
```

## Configuring k3s containerd to use an insecure registry

To fix that, we need to configure containerd. For k3s, that's documented
[here](https://rancher.com/docs/k3s/latest/en/installation/private-registry/).

We need to create a file `/etc/rancher/k3s/registries.yaml` on each node,
as below. Note that the directory might not exist on the worker nodes:

```bash
sudo mkdir -p /etc/rancher/k3s/
sudo chmod 755 /etc/rancher/k3s/
sudo vi /etc/rancher/k3s/registries.yaml  # as follows
```

```yaml
mirrors:
  "10.43.236.176:5000":
    endpoint:
      - "http://10.43.236.176:5000"
```

The mirror name is the name of our registry. For now, I'm going to use
the dotted-IP, because I don't want to get into naming things until
later.

I'm probably going to run into problems because this is a ClusterIP, but
this is all about the learning.

The endpoint uses `http://`, which disables the use of TLS.

According to the documentation, we can add multiple endpoints, which
seems like a reasonable way to avoid needing a load balancer.

This file needs to be deployed on all nodes where it will be used, and
the k3s service needs to be restarted:

```bash
sudo systemctl restart k3s    # on the master
```

```bash
sudo systemctl restart k3s-agent  # on the workers
```

Obviously, if you were using this in production, you'd migrate the load
off the workers, and you'd be using HA master-only nodes, right?

Equally obviously, if you were using this in production, you wouldn't be
avoiding TLS, and you'd have working DNS...

## Try it again

```bash
$ kubectl apply -f hello-world.yml
deployment.apps/hello-world created
```

```
$ kubectl --namespace docker-registry get all
NAME                                   READY   STATUS             RESTARTS   AGE
pod/docker-registry-684dc65c99-pckxl   1/1     Running            0          6d5h
pod/hello-world-5449677b5b-7wwtm       0/1     CrashLoopBackOff   1          20s
```

Ooh. `CrashLoopBackOff`. That's different. Is that good?

```
$ kubectl --namespace docker-registry describe pod/hello-world-5449677b5b-7wwtm
Name:         hello-world-5449677b5b-7wwtm
...
Events:
  Type     Reason     Age                From               Message
  ----     ------     ----               ----               -------
  Normal   Scheduled  61s                default-scheduler  Successfully assigned docker-registry/hello-world-5449677b5b-7wwtm to rpi404
  Normal   Pulled     60s                kubelet            Successfully pulled image "10.43.236.176:5000/hello-world" in 543.718498ms
  Normal   Pulled     59s                kubelet            Successfully pulled image "10.43.236.176:5000/hello-world" in 250.715721ms
  Normal   Pulled     42s                kubelet            Successfully pulled image "10.43.236.176:5000/hello-world" in 190.243966ms
  Normal   Pulling    18s (x4 over 61s)  kubelet            Pulling image "10.43.236.176:5000/hello-world"
  Normal   Pulled     18s                kubelet            Successfully pulled image "10.43.236.176:5000/hello-world" in 203.508818ms
  Normal   Created    18s (x4 over 60s)  kubelet            Created container hello-world
  Normal   Started    17s (x4 over 59s)  kubelet            Started container hello-world
  Warning  BackOff    3s (x6 over 58s)   kubelet            Back-off restarting failed container
```

This bit right here: `Successfully pulled image "10.43.236.176:5000/hello-world" in 203.508818ms`. Yes, it's good.

We still need to clean up, though:

```
$ kubectl --namespace docker-registry delete deployment hello-world
```

## Recap

So far, we've got a private docker registry running in a pod, and we can use it to provide images to our nodes.

But there are problems, and they all come down to the same two things: DNS and load-balancing.

- The name changes every time we do a deploy.
- We can't push images from outside the cluster.
- If we use a NodePort service, that improves matters slightly:
  - The name changes if the pod moves to a different node.
  - We can add multiple endpoints in `registries.yaml`, which would help.
  - We can deal with that for external pushes if we're prepared to look up the IP
    address each time.
- We _really_ ought to be using TLS.
  - But we can't do that if the name's going to keep changing.
- Docker image tags have the registry name in them.
  - Currently this is the dotted IP address.
  - We can use a fixed mirror name in `registries.yaml` to deal with that.
  - But that won't work outside the cluster.
  - If the registry name changes, does that break the image tags?

We can work around these problems for a while, but at some point we'll need to
bite the bullet. We're getting there.
