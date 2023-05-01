---
title: "Erlang on Kubernetes: Using Kustomize to manage a ConfigMap for sys.config"
date: 2023-05-01T13:35:00.000Z
tags: erlang kubernetes kustomize
---

In a [previous post]({% post_url 2023/2023-04-13-erlang-k8s-configmap %}), I showed how to store `sys.config` and
`vm.args` in a ConfigMap. There are some problems with that approach, so I'm going to use Kustomize to fix that.

## Why use a ConfigMap?

Ordinarily, the `sys.config` file is stored in the container images. This makes it hard to vary the configuration at
deploy time. For example, you might have separate dev, stage and production clusters, and each needs a slightly
different configuration.

This was my motivation for storing `sys.config` in a ConfigMap -- it can be easily varied between different clusters.

## Why not use a ConfigMap?

But: using a single ConfigMap introduces other problems. If you change it independently from the deployment, you can end
up with old pods using the new configuration, and vice versa.

For example, consider the following:

1. You want to deploy a new version of your app, which requires some configuration changes.
2. You make the appropriate changes to the ConfigMap, and start deploying the new version. It fails, so you decide to
   rollback.
3. While this is happening, a node dies and some of the older pods are restarted.
4. They pick up the new ConfigMap, but they don't understand it, and they start failing.
5. Whoops.

## Kustomize

Kustomize has a feature whereby it puts a unique hash in the ConfigMap name, effectively tying it to a particular
deployment. I'm going to use this.

To do this, I'll create `kustomization.yaml`, as follows:

```yaml
namespace: erlclu
resources:
  - cleanup-cronjob.yaml
  - deployment.yaml
  - headless-service.yaml
  - ingress.yaml
  - issuer.yaml
  - role-binding.yaml
  - role.yaml
  - service-account.yaml
  - service.yaml
  - vm-scraper.yaml
configMapGenerator:
  - name: erlclu-config
    files:
      - config/vm.args.src
      - config/sys.config
```

Note that kustomize (deliberately) doesn't support globbing; the list of resources must be manually-managed. Kinda
sucks, but it is what it is.

Specifically, I discovered this when using ArgoCD to manage the application; it wanted to prune the resources that
weren't explicitly listed. So be careful of that. The "needs pruning" and "out of date" icons in the ArgoCD UI are very
similar.

Then I need to create the files in the `config` directory; they're just copies of the ones from the container image for
now.

And then it needs applying:

```sh
kubectl apply -k k8s/
```

Another point about ArgoCD and pruning: don't allow it to prune the old configmap until there are no deployments that
depend on it. You might want to roll back the deployment, after all.
