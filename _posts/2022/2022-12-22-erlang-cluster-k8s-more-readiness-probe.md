---
title: "Erlang cluster on Kubernetes: More Readiness Probe"
short_title: "More Readiness Probe"
date: 2022-12-22T21:24:00.000Z
layout: series
series: erlang-cluster-k8s
tags: erlang kubernetes
published: false
---

Even after fixing this, I still -- very occasionally -- see the same error when a request goes to a pod that's just been
terminated. I found a page --
<https://azure.github.io/application-gateway-kubernetes-ingress/how-tos/minimize-downtime-during-deployments/> -- that
suggests a way to fix that, which I'll look into later.

https://about.gitlab.com/blog/2022/05/17/how-we-removed-all-502-errors-by-caring-about-pid-1-in-kubernetes/ talks about a pod not being removed from the endpoint until it fails a certain number of readiness probes; maybe tweaking the frequency would help. But I'm not seeing the error enough to suggest that's an issue. So maybe it's the Azure thing.

That also talks about PID 1 and sending TERM signals. I fixed that stuff later. So maybe it's fixed?

It's not; it's probably the Azure thing.

https://github.com/lnx-search/rewrk

```yaml
monitoring-system/traefik [main ?1] % cat scraper.yaml
apiVersion: operator.victoriametrics.com/v1beta1
kind: VMPodScrape
metadata:
  name: traefik
  namespace: monitoring-system
spec:
  namespaceSelector:
    matchNames:
      - "kube-system"
  selector:
    matchLabels:
      app.kubernetes.io/name: traefik
  podMetricsEndpoints:
    - port: metrics
      path: /metrics
      relabelConfigs:
        - sourceLabels:
            - "__meta_kubernetes_pod_node_name"
          action: replace
          targetLabel: instance
```

https://ellispritchard.medium.com/graceful-shutdown-on-kubernetes-with-signals-erlang-otp-20-a22325e8ae98

https://github.com/Financial-Times/k8s_traffic_plug

https://www.erlang.org/doc/man/kernel_app.html#os-signal-event-handler

https://www.erlang.org/doc/man/application.html#Module:prep_stop-1

https://kubernetes.io/docs/tasks/configure-pod-container/configure-liveness-readiness-startup-probes/
