---
title: "apt upgrade on K3s node"
date: 2023-01-07T09:19:00.000Z
tags: k3s ubuntu runbook
---

Given [the problems]({% post_url 2022/2022-12-15-k3s-pv-loss-incident-review %}) I had when I last upgraded everything
on my K3s cluster, I'm going to put a runbook together for doing it "properly".

## Drain the server

```
% kubectl drain roger-nuc0
node/roger-nuc0 cordoned
error: unable to drain node "roger-nuc0" due to error:[cannot delete Pods with local storage (use --delete-emptydir-data to override): ..., cannot delete DaemonSet-managed Pods (use --ignore-daemonsets to ignore): ..., cannot delete Pods declare no controller (use --force to override): default/dnsutils], continuing command...
There are pending nodes to be drained:
 roger-nuc0
...
```

So it's cordoned the node, but it can't drain it. This is annoying, because it won't even start draining the node until
you resolve _all_ of the problems. Let's try again. First we'll delete the uncontrolled pod:

```
% kubectl delete pod dnsutils
pod "dnsutils" deleted
```

Then we'll run the command again, adding the suggested options:

```
% kubectl drain roger-nuc0 --ignore-daemonsets --delete-emptydir-data
node/roger-nuc0 already cordoned
Warning: ignoring DaemonSet-managed Pods: ...
evicting pod grafana/grafana-5f7f6d4d8c-lksrj
...
```

## Checks

At this point, I'm going to check that my cluster's still working. This is probably paranoia, and should be automated.

- The Longhorn dashboard shows that the volumes are all still healthy.
- Grafana is still working. There's a brief gap while various pods were being evicted, but everything looks basically OK.
- ArgoCD looks healthy. Refreshing the apps succeeded, which implies that Gitea is still OK.
- I can log into Gitea and everything seems to be OK.

```sh
kubectl get volumes.longhorn.io -A
```

```sh
argocd app list -o json | \
    jq -r '.[].metadata.name' | \
    xargs -I{} \
        argocd app get {} --refresh
# ...but I haven't figured out how to assert the status yet.
```

## Upgrade the server

```
sudo apt update
sudo apt upgrade
sudo shutdown -r now
```

And then we wait.

## Upgrade the agents

Repeat for the agent nodes. I did them in reverse order: `roger-nuc3`, `roger-nuc2`, `roger-nuc1`.

## Related Links

- <https://pet2cattle.com/2021/08/cannot-delete-pods-with-local-storage>
