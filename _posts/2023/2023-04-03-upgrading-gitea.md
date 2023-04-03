---
title: "Upgrading gitea"
date: 2023-04-03T15:53:00Z
tags: kubernetes gitea
---

I'm running Gitea and ArgoCD on my K3s cluster, for some GitOps goodness. I noticed that Gitea 1.19.0 recently came out,
with some features I want to try, such as Gitea Actions. Since I'm running 1.17.4, it's time to upgrade.

Hopefully it's as simple as the following:

```sh
helm repo update
helm upgrade gitea gitea-charts/gitea --namespace gitea --values values.yaml
```

...where `values.yaml` is from my original installation.

_...10 minutes later..._

Yep. Seems to have worked.
