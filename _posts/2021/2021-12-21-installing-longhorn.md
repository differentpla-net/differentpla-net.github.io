---
title: "Installing Longhorn"
date: 2021-12-21T11:28:00Z
tags: raspberry-pi longhorn k3s
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

<div class="callout callout-warning" markdown="span">
Restarting a node (to install Ubuntu updates, e.g.) causes the `local-path` storage class to be re-registered as the default.
See [this issue](https://github.com/k3s-io/k3s/issues/3441). Consider disabling it entirely, or just be aware that occasionally
you'll get `Internal error occurred: 2 default StorageClasses were found` errors, and you'll have to patch it again.
</div>

## User Interface

The Longhorn frontend is accessible through a ClusterIP endpoint:

```
$ kubectl --namespace longhorn-system get service longhorn-frontend
NAME                TYPE        CLUSTER-IP      EXTERNAL-IP   PORT(S)   AGE
longhorn-frontend   ClusterIP   10.43.174.121   <none>        80/TCP    9d
```

We can expose this in a number of different ways:

### kubectl port-forward

```
$ kubectl --namespace longhorn-system port-forward --address 0.0.0.0 service/longhorn-frontend 5080:80
```

This makes the UI accessible at <http://rpi401:5080/#/dashboard>. If you're running `kubectl` _on_ one of the nodes,
you'll need the `--address 0.0.0.0` option, but note that this is insecure: it exposes the UI to the local network. If
you're running `kubectl` on your desktop, you can omit the `--address 0.0.0.0`, and the port will be opened to
`localhost` only.

### ssh port forward

```
$ ssh -L5080:10.43.174.121:80 ubuntu@rpi401
```

This makes the UI accessible at <http://localhost:5080/#/dashboard>. It's more secure (localhost only, and the traffic
goes over SSH), but it requires figuring out the ClusterIP, rather than just specifying the service name.

<div class="callout callout-info" markdown="span">
Tip: You can use an SSH escape sequence to forward the port without disconnecting. Press `Enter`, then `~C`, then `L5080:10.43.174.121:80`, then `Enter`.
</div>
