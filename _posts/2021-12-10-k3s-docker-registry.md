---
title: "k3s on Raspberry Pi: Docker Registry"
date: 2021-12-10T09:49:00Z
layout: series
series: k3s
tags: k3s raspberry-pi
---

In order to deploy our own applications, we're going to need a private docker repository.
When I [first did this]({% post_url 2020-02-06-k3s-raspi-private-docker %}), I used a standalone container.

This time, I'm going to run the registry "properly", inside the k8s cluster. I'm going to start
with a basic docker container and add features as we go along.

## The basics

We'll need a deployment:

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
```

Note that it uses the `docker-registry` namespace, so we'll need to create that:

```
kubectl create namespace docker-registry
```

Then we can create the deployment:

```
kubectl apply -f docker-registry.yml
```

And we can see it running:

```
$ kubectl --namespace docker-registry get all
NAME                                   READY   STATUS    RESTARTS   AGE
pod/docker-registry-684dc65c99-pckxl   1/1     Running   0          4s

NAME                              READY   UP-TO-DATE   AVAILABLE   AGE
deployment.apps/docker-registry   1/1     1            1           4s

NAME                                         DESIRED   CURRENT   READY   AGE
replicaset.apps/docker-registry-684dc65c99   1         1         1       4s
```

## Services

We need to make it accessible to the other nodes in the cluster, and -- if we want to push images from elsewhere -- outside the cluster.

At some point I'll figure out how to get load balancing and/or ingress to work. For now, let's just use _ClusterIP_ and _NodePort_ settings.

We can expose the service with some relatively sane defaults as follows:

```
$ kubectl --namespace docker-registry expose deploy docker-registry
service/docker-registry exposed

$ kubectl --namespace docker-registry get service
NAME              TYPE        CLUSTER-IP    EXTERNAL-IP   PORT(S)    AGE
docker-registry   ClusterIP   10.43.241.2   <none>        5000/TCP   2m12s
```

And then we can access it:

```
$ curl http://10.43.241.2:5000/v2/_catalog
{"repositories":[]}
```

What we _should_ have done, however, is created the service with a YAML file.
We can grab the existing definition:

```
kubectl --namespace docker-registry get service docker-registry -o yaml > service.yml
```

And then we can edit the file to just the required bits:

```yaml
apiVersion: v1
kind: Service
metadata:
  labels:
    app: docker-registry
  name: docker-registry
  namespace: docker-registry
spec:
  ports:
  - port: 5000
    protocol: TCP
    targetPort: 5000
  selector:
    app: docker-registry
  type: ClusterIP
```

When we try to apply it, we get a warning:

```
$ kubectl apply -f service.yml
Warning: resource services/docker-registry is missing the kubectl.kubernetes.io/last-applied-configuration annotation which is required by kubectl apply. kubectl apply should only be used on resources created declaratively by either kubectl create --save-config or kubectl apply. The missing annotation will be patched automatically.
service/docker-registry configured
```

What it's complaining about is that we shouldn't have used `kubectl apply` to overwrite a resource that wasn't originally created with `kubectl apply`.

It uses the `kubectl.kubernetes.io/last-applied-configuration` to tell the difference.

Does it still work? Note that the IP address has changed:

```
$ curl http://10.43.236.176:5000/v2/_catalog
{"repositories":[]}
```

Yes. Good.

## Pushing an image

Let's see if we can push an image to it. First we need to install the Docker CLI.

Because this is Raspbian, we need to use the "convenience" script that Docker provides. See [Install using the convenience script](https://docs.docker.com/engine/install/debian/#install-using-the-convenience-script).

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

```bash
$ sudo docker pull hello-world
Using default tag: latest
latest: Pulling from library/hello-world
9b157615502d: Pull complete
Digest: sha256:cc15c5b292d8525effc0f89cb299f1804f3a725c8d05e158653a563f15e4f685
Status: Downloaded newer image for hello-world:latest
docker.io/library/hello-world:latest

$ sudo docker tag hello-world 10.43.236.176:5000/hello-world

$ sudo docker push 10.43.236.176:5000/hello-world
Using default tag: latest
The push refers to repository [10.43.236.176:5000/hello-world]
Get "https://10.43.236.176:5000/v2/": http: server gave HTTP response to HTTPS client
```

Ah. That's a problem. Can we tell docker to not use HTTPS for now?

Yes, you can. See [this Stack Overflow question](https://stackoverflow.com/q/49674004/).

But don't do that, because, well, read on...

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

Yeah, we get the same `server gave HTTP response to HTTPS client` error as before,
and the [last time I tried this]({% post_url 2020-02-06-k3s-raspi-private-docker %}), I gave up trying to get it to work and just created TLS certificates instead.

So we'll aim to do that next. I suspect that there's going to be [a diversion
into DNS first]({% post_url 2021-12-10-k3s-docker-registry-whats-in-a-name %}), though.
