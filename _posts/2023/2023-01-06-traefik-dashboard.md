---
title: "Traefik dashboard on K3s"
date: 2023-01-06T17:24:00.000Z
tags: traefik k3s
---

It turns out that Traefik has a dashboard. Here's how to access it via `kubectl port-forward`.

```sh
kubectl --namespace kube-system port-forward deployments/traefik 9000:9000 &
```

Then navigate to <http://localhost:9000/dashboard/>.

If you want to make this more permanent by using an Ingress, see <https://k3s.rocks/traefik-dashboard/>.
