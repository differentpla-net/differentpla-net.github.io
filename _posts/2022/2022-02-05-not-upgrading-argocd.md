---
title: "(Not) Upgrading ArgoCD"
date: 2022-02-05T11:47:00Z
tags: argo-cd kubernetes
layout: series
series: k3s
---

There's a [security fix](https://www.theregister.com/2022/02/04/argo_cd_0day_kubernetes/) that needs to be applied;
there's an [arm64 release candidate](https://github.com/argoproj/argo-cd/releases/tag/v2.3.0-rc5). Time to upgrade
ArgoCD.

{% capture message %}
This fails; keep an eye on [argo-cd#8394](https://github.com/argoproj/argo-cd/issues/8394).
{% endcapture %}
{% include alerts/danger.html content=message %}

It _ought_ to be as simple as:

```
kubectl apply -n argocd -f https://raw.githubusercontent.com/argoproj/argo-cd/v2.3.0-rc5/manifests/install.yaml
```

...and it _mostly_ was. The only problem was this:

```
pod/argocd-applicationset-controller-9488fc486-jjt98   0/1     CrashLoopBackOff   4 (61s ago)   4m6s
```

Let's take a look:

```
$ kubectl --namespace argocd logs argocd-applicationset-controller-9488fc486-jjt98
standard_init_linux.go:228: exec user process caused: exec format error
```

Sigh. I filed a bug: [argo-cd#8394](https://github.com/argoproj/argo-cd/issues/8394).

I attempted to roll back:

```
$ kubectl --namespace argocd rollout undo deployment/argocd-redis
deployment.apps/argocd-redis rolled back

$ kubectl --namespace argocd rollout undo deployment/argocd-repo-server
deployment.apps/argocd-repo-server rolled back

$ kubectl --namespace argocd rollout undo deployment/argocd-server
deployment.apps/argocd-server rolled back

$ kubectl --namespace argocd rollout undo deployment/argocd-notifications-controller
error: no rollout history found for deployment "argocd-notifications-controller"

$ kubectl --namespace argocd rollout undo deployment/argocd-dex-server
deployment.apps/argocd-dex-server rolled back

$ kubectl --namespace argocd rollout undo deployment/argocd-applicationset-controller
deployment.apps/argocd-applicationset-controller rolled back

$ kubectl --namespace argocd rollout undo statefulset/argocd-application-controller
statefulset.apps/argocd-application-controller rolled back
```

...but it didn't work, and I don't yet know enough about Kubernetes to figure out the problem, so I just deleted all of
the deployments and stateful sets...

```
$ kubectl --namespace argocd delete statefulset argocd-application-controller
statefulset.apps "argocd-application-controller" deleted

$ kubectl --namespace argocd delete deployment argocd-notifications-controller
deployment.apps "argocd-notifications-controller" deleted

$ kubectl --namespace argocd delete deployment argocd-redis
deployment.apps "argocd-redis" deleted

$ kubectl --namespace argocd delete deployment argocd-repo-server
deployment.apps "argocd-repo-server" deleted

$ kubectl --namespace argocd delete deployment argocd-dex-server
deployment.apps "argocd-dex-server" deleted

$ kubectl --namespace argocd delete deployment argocd-applicationset-controller
deployment.apps "argocd-applicationset-controller" deleted

$ kubectl --namespace argocd delete deployment argocd-server
deployment.apps "argocd-server" deleted
```

...and reapplied the [previous install manifest]({% post_url 2022/2022-02-02-argocd %}). That got me back to a working
state.
