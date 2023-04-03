---
title: "Using MetalLB CRDs"
date: 2023-04-03T15:33:00Z
tags: kubernetes metallb
---

When I originally [installed MetalLB]({% post_url 2021/2021-12-20-installing-metallb %}), it used a `ConfigMap` for
setting up address ranges. Since 0.13.2, it supports configuration using Custom Resource Definitions (CRDs). I forgot to
write a blog post about that when I upgraded.

The `IPAddressPool` looks like this:

```yaml
apiVersion: metallb.io/v1beta1
kind: IPAddressPool
metadata:
  name: default
  namespace: metallb-system
spec:
  addresses:
  - 192.168.28.60-192.168.28.90
```

If you're using ARP for address discovery, you'll need an `L2Advertisement` as well:

```yaml
apiVersion: metallb.io/v1beta1
kind: L2Advertisement
metadata:
  name: default
  namespace: metallb-system
```

It works because both the `IPAddressPool` and `L2Advertisement` are named "default".
