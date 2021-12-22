---
title: "Installing k3s on Ubuntu on Raspberry Pi"
date: 2021-12-20T19:16:00Z
layout: series
series: k3s
tags: raspberry-pi k3s ubuntu
---

Having reinstalled all of my nodes with Ubuntu, I need to go back and install k3s. Joy.

## Install needed modules

Per [k3s#4234](https://github.com/k3s-io/k3s/issues/4234), k3s requires some extra modules that aren't installed by default on Ubuntu.

```
sudo apt install linux-modules-extra-raspi
```

## Install k3s control plane

Following the instructions at <https://rancher.com/docs/k3s/latest/en/quick-start/>

On the first node:

```bash
curl -sfL https://get.k3s.io | sh -
```

Then wait, occasionally running `sudo k3s kubectl get nodes` to check on progress.

## Install k3s agents

Once that's completed, grab the server token:

```bash
sudo cat /var/lib/rancher/k3s/server/node-token
```

Then on the other nodes:

```bash
curl -sfL https://get.k3s.io | K3S_URL=https://rpi401:6443 K3S_TOKEN=<node-token> sh -
```

## KUBECONFIG

Back on the primary node:

```bash
mkdir ~/.k3s
sudo cp /etc/rancher/k3s/k3s.yaml ~/.k3s/k3s.yaml
sudo chown $USER.$USER ~/.k3s/k3s.yaml
export KUBECONFIG=$HOME/.k3s/k3s.yaml   # add this to ~/.bashrc
```

## Shell auto-completion

Per <https://kubernetes.io/docs/tasks/tools/included/optional-kubectl-configs-bash-linux/>

```bash
echo 'source <(kubectl completion bash)' >>~/.bashrc
```

## Is it working?

```bash
$ kubectl get nodes
NAME     STATUS   ROLES                  AGE   VERSION
rpi401   Ready    control-plane,master   80m   v1.21.7+k3s1
rpi405   Ready    <none>                 28s   v1.21.7+k3s1
rpi404   Ready    <none>                 18s   v1.21.7+k3s1
rpi403   Ready    <none>                 18s   v1.21.7+k3s1
rpi402   Ready    <none>                 32s   v1.21.7+k3s1
```
