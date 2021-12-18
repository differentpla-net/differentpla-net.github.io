---
title: "k3s on Raspberry Pi: Persistent Volume Namespaces"
short_title: "Persistent Volume Namespaces"
date: 2021-12-18T20:11:00Z
layout: series
series: k3s
tags: k3s raspberry-pi
---

Persistent Volume Claims are namespace-scoped. Persistent Volumes are not:

```
$ kubectl get pvc
NAME              STATUS   VOLUME           CAPACITY   ACCESS MODES   STORAGECLASS   AGE
testing-vol-pvc   Bound    testing-vol-pv   1Gi        RWO            iscsi          27h

$ kubectl --namespace docker-registry  get pvc
NAME                  STATUS   VOLUME               CAPACITY   ACCESS MODES   STORAGECLASS   AGE
docker-registry-pvc   Bound    docker-registry-pv   1Gi        RWO            iscsi          23h

$ kubectl get pv
NAME                 CAPACITY   ACCESS MODES   RECLAIM POLICY   STATUS   CLAIM                                 STORAGECLASS   REASON   AGE
testing-vol-pv       1Gi        RWO            Retain           Bound    default/testing-vol-pvc               iscsi                   27h
docker-registry-pv   1Gi        RWO            Retain           Bound    docker-registry/docker-registry-pvc   iscsi                   23h
```
