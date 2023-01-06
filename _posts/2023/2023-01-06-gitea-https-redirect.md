---
title: "Redirecting HTTP to HTTPS on Gitea"
date: 2023-01-06T17:54:00.000Z
tags: gitea kubernetes
---

Installed gitea with HTTPS. Link to that. Because of SSH + HTTPS, using a load balancer. Means that can't use Ingress / IngressRoute.

Helm upgrade.

helm upgrade gitea gitea-charts/gitea --namespace gitea --values values.yaml

Edit values.yaml according to https://docs.gitea.io/en-us/https-setup/

How do the port numbers even work anyway?
