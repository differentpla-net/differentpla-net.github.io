---
title: "Installing Helm"
date: 2021-12-20T20:45:00Z
tags: raspberry-pi
layout: series
series: k3s
---

To install other things, we're going to want to use Helm. So let's install that first.

The latest release is available at <https://github.com/helm/helm/releases/tag/v3.7.2>.

```
wget https://get.helm.sh/helm-v3.7.2-linux-arm64.tar.gz
tar xfz helm-v3.7.2-linux-arm64.tar.gz
sudo mv linux-arm64/helm /usr/local/bin
helm version
```
