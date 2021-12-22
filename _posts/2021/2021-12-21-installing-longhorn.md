---
title: "Installing Longhorn"
date: 2021-12-21T11:28:00Z
tags: raspberry-pi
layout: series
series: k3s
---

Installation with Helm, per <https://longhorn.io/docs/1.2.3/deploy/install/install-with-helm/>.

## Add the repo

```
helm repo add longhorn https://charts.longhorn.io
helm repo update
```

## Run the installation

```bash
helm install longhorn longhorn/longhorn --namespace longhorn-system --create-namespace
```

## Make Longhorn the default storage class

Per <https://rpi4cluster.com/k3s/k3s-storage-setting/#make-longhorn-the-default-storageclass>:

```
$ kubectl get storageclass
NAME                   PROVISIONER             RECLAIMPOLICY   VOLUMEBINDINGMODE      ALLOWVOLUMEEXPANSION   AGE
local-path (default)   rancher.io/local-path   Delete          WaitForFirstConsumer   false                  20h
longhorn (default)     driver.longhorn.io      Delete          Immediate              true                   16m
```

```
$ kubectl patch storageclass local-path -p '{"metadata": {"annotations":{"storageclass.kubernetes.io/is-default-class":"false"}}}'
```

```
$ kubectl get storageclass
NAME                 PROVISIONER             RECLAIMPOLICY   VOLUMEBINDINGMODE      ALLOWVOLUMEEXPANSION   AGE
longhorn (default)   driver.longhorn.io      Delete          Immediate              true                   19m
local-path           rancher.io/local-path   Delete          WaitForFirstConsumer   false                  20h
```
