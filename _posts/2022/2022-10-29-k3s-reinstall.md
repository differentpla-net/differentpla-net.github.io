---
title: "Reinstalling k3s"
date: 2022-10-29T10:34:00Z
tags: k3s kubernetes
layout: series
series: k3s-reinstall
---

I'm going to rebuild my k3s cluster with some more capable hardware.

In the [previous series]({% post_url 2020/2020-02-06-k3s-raspi-intro %}), I installed a k3s cluster on some Raspberry Pi
nodes. Initially, I used a stack of RPi 2B nodes; then I upgraded to a stack of RPi 4B nodes.

Unfortunately, I've been having problems with Longhorn stability on that hardware -- they're just too underpowered, in
terms of both network bandwidth and USB storage speed. I considered adding external SSDs to the nodes to see if that
would improve things, but in the end I decided that the point of this exercise is to play with Kubernetes, not to fiddle
with underpowered hardware.

So, I'm going to build a new cluster on a pair of Intel NUCs that are sitting on my desk.

This series will initially be a whistlestop recap of the previous one as I bring everything back up to parity. Maybe
I'll have some more posts to add later. We'll see.

curl -sfL https://get.k3s.io | INSTALL_K3S_EXEC="--disable servicelb" sh -

k3s is 1.25.0, which doesn't work with Longhorn 1.3.2 stable; so install from master. I'll use helm to (hopefully) make it easier to upgrade to Longhorn 1.4.0 later.

https://github.com/longhorn/longhorn/issues/4003#issuecomment-1291280076

curl -sSfL https://raw.githubusercontent.com/longhorn/longhorn/v1.3.2/scripts/environment_check.sh | bash

```
git clone https://github.com/longhorn/longhorn.git
cd longhorn
helm install longhorn ./chart --namespace longhorn-system --create-namespace
```


- [Installing k3s on Ubuntu]({% post_url 2021/2021-12-20-k3s-ubuntu-reinstall %}); we don't need the raspi modules.
- [Install Helm]({% post_url 2021/2021-12-20-installing-helm %}); current version is v3.10.1; we want amd64, rather then arm64.
- [Install MetalLB]({% post_url 2022/2022-10-29-reinstall-metallb %})
- [Install Longhorn]({% post_url 2021/2021-12-21-installing-longhorn %})

## CertManager

https://blog.differentpla.net/blog/2022/02/06/cert-manager/

kubectl apply -f https://github.com/cert-manager/cert-manager/releases/download/v1.10.0/cert-manager.yaml

{% include _series_toc.html %}

```bash
{% raw %}kubectl --namespace monitoring-system \
  get secret vmagent-default \
  --template="{{index .data \"vmagent.yaml.gz\" | base64decode}}" \
  | gunzip -c{% endraw %}
```
