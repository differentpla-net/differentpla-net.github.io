---
title: "Upgrading MetalLB"
date: 2023-04-16T16:26:00Z
tags: kubernetes metallb
---

I installed MetalLB using helm. Upgrading is a one-liner.

```bash
helm --namespace metallb-system upgrade metallb metallb/metallb
```
