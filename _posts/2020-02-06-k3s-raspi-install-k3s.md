---
title: "k3s on Raspberry Pi: Installing k3s"
short_title: "Installing k3s"
date: 2020-02-06T17:04:00
layout: series
series: k3s
tags: k3s raspberry-pi
---

It's at this point that I diverge from Scott's blog post; he installs full-fat Kubernetes. I'm going to use [k3s](https://k3s.io/).

This is taken from the [Quick-Start Guide](https://rancher.com/docs/k3s/latest/en/quick-start/).

On the master node, run the following:

```
curl -sfL https://get.k3s.io | sh -
```

The daemon takes several minutes to come up, so be patient.

I don't recall whether you need to explicitly start the daemon, or if it'll start automatically.

Get the server's token:

```
sudo cat /var/lib/rancher/k3s/server/node-token
```

On the agent nodes, run the following:

```
curl -sfL https://get.k3s.io | \
    K3S_URL=https://rpi201:6443 \
    K3S_TOKEN=K... \
    sh -
```

Back on the master node:

```
$ sudo kubectl get nodes
NAME     STATUS   ROLES    AGE   VERSION
rpi204   Ready    <none>   28h   v1.17.2+k3s1
rpi203   Ready    <none>   28h   v1.17.2+k3s1
rpi205   Ready    <none>   28h   v1.17.2+k3s1
rpi201   Ready    master   28h   v1.17.2+k3s1
rpi202   Ready    <none>   28h   v1.17.2+k3s1
```
