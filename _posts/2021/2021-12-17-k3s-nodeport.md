---
title: "k3s on Raspberry Pi: NodePort"
short_title: "NodePort"
date: 2021-12-17T20:58:00Z
layout: series
series: k3s
tags: k3s raspberry-pi
---

Until just now, I didn't _get_ `NodePort` services.

The [Kubernetes documentation][publishing-services-service-types] says the following:

> `NodePort`: Exposes the Service on each Node's IP at a static port (the `NodePort`). A `ClusterIP` Service, to which
> the `NodePort` Service routes, is automatically created. You'll be able to contact the `NodePort` Service, from
> outside the cluster, by requesting `<NodeIP>:<NodePort>`.

This page -- [Kubernetes: ClusterIP, NodePort, or Ingress? When to Use Each][when-to-use-each] -- finally made it
click:

> Creating a NodePort will open that port on every node in your cluster. Kubernetes will automatically route port
> traffic to the service it’s linked to.

_That's_ why it insists on reserving a range of port numbers -- it needs to find one that's available on _every_ node in
the cluster.

Let's add a `NodePort` service to our docker registry:

```yaml
apiVersion: v1
kind: Service
metadata:
  labels:
    app: docker-registry
  name: docker-registry-np
  namespace: docker-registry
spec:
  selector:
    app: docker-registry
  type: NodePort
  ports:
  - port: 5000
    targetPort: 5000
```

```
$ kubectl apply -f service.yml
service/docker-registry unchanged
service/docker-registry-np created
```

```
$ kubectl --namespace docker-registry get services
NAME                 TYPE        CLUSTER-IP      EXTERNAL-IP   PORT(S)          AGE
docker-registry      ClusterIP   10.43.236.176   <none>        5000/TCP         7d8h
docker-registry-np   NodePort    10.43.191.198   <none>        5000:30721/TCP   39s
```

What used to confuse me here is that I've already got a `CLUSTER-IP`, now I've got another one? I thought this was
supposed to be accessible externally, but there's no `EXTERNAL-IP` listed. What's with the weird `5000:30721` thing? Why
is it the "wrong" way round?

What's going on is that you can access port 30721 from outside the cluster to _any_ cluster node, and Kubernetes will
transparently route your traffic to the relevant service.

Until now, I've been assuming that "Exposes the Service on each Node's IP" meant _each node that the container is
running on_. I've been using `describe pod` to figure that out first before attempting to talk to my service.

But, no, we can do this:

```
desktop-pc % curl http://rpi401:30721:/v2/_catalog
{"repositories":[]}

desktop-pc % curl http://rpi405:30721:/v2/_catalog
{"repositories":[]}

desktop-pc % curl http://rpi403:30721:/v2/_catalog
{"repositories":[]}
```

At this point, I don't even know which node my docker registry is running on (actually I do, it's `rpi405`).

OK, so it's not a load-balancer, but it's not bad.

[publishing-services-service-types]: https://kubernetes.io/docs/concepts/services-networking/service/#publishing-services-service-types
[when-to-use-each]: https://www.cloudsavvyit.com/11261/kubernetes-clusterip-nodeport-or-ingress-when-to-use-each/
