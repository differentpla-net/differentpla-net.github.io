---
title: "Using k3s on Raspberry Pi: Installing nginx"
date: 2020-02-06T17:44:00
layout: series
series: k3s
tags: k3s raspberry-pi
---

Start nginx, with 3 replicas:

```
$ sudo kubectl run nginx-1 --image=nginx --replicas=3 --port=80
```

You can see that 3 "pods" have been started:

```
$ sudo kubectl get pods
NAME                          READY   STATUS    RESTARTS   AGE
nginx-1-775985c86-m7ktb       1/1     Running   0          12s
nginx-1-775985c86-z26d2       1/1     Running   0          12s
nginx-1-775985c86-m7x75       1/1     Running   0          12s
```

Then you need to make nginx available. This uses the default 'ClusterIP' type. I don't know enough about the other options to make an intelligent choice here.

```
$ sudo kubectl expose deployment nginx-1 --port 80
$ sudo kubectl get endpoints nginx-1
NAME      ENDPOINTS                                   AGE
nginx-1   10.42.1.15:80,10.42.2.15:80,10.42.3.13:80   5h20m
```

As you can see, there are three different endpoints -- one for each pod. They're using cluster-internal IP addresses.

This allows us to access one of the instances from inside the cluster:

```
$ curl http://10.42.1.15
<!DOCTYPE html>
<html>
<head>
<title>Welcome to nginx!</title>
```

## Optional: Clean up

```
$ sudo kubectl delete service nginx-1
service "nginx-1" deleted
$ sudo kubectl delete deployment nginx-1
deployment.apps "nginx-1" deleted
```

## Links

- <http://alesnosek.com/blog/2017/02/14/accessing-kubernetes-pods-from-outside-of-the-cluster/>
- <https://medium.com/@mabrams_46032/you-can-expose-services-in-the-standard-k8s-manner-for-example-9ad33924b67e>
- <https://www.ovh.com/blog/getting-external-traffic-into-kubernetes-clusterip-nodeport-loadbalancer-and-ingress/>
