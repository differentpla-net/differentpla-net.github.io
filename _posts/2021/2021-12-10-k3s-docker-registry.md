---
title: "k3s on Raspberry Pi: Private Docker Registry"
short_title: "Private Docker Registry"
date: 2021-12-10T09:49:00Z
layout: series
series: k3s
tags: k3s raspberry-pi
---

{% capture info %}
**Updated:** See [Docker Registry Redux]({% post_url 2021/2021-12-21-docker-registry-redux %}).
{% endcapture %}
{% include alerts/light.html content=info %}

In order to deploy our own applications, we're going to need a private docker repository.
When I [first did this]({% post_url 2020/2020-02-06-k3s-raspi-private-docker %}), I used a standalone container.

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
