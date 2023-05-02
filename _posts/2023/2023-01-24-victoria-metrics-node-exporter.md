---
title: "Using node-exporter with VictoriaMetrics"
date: 2023-01-24T18:37:00Z
tags: victoria-metrics
---

I'd like to get some CPU temperature metrics from the nodes in my K3s cluster. That's the job of Prometheus'
`node-exporter`. I'm not using Prometheus; I'm using VictoriaMetrics. Here's how to get it working.

The first thing we need is a DaemonSet, so that the node-exporter is running on each node. This is lifted from [here](https://devopscube.com/node-exporter-kubernetes/).

```yaml
apiVersion: apps/v1
kind: DaemonSet
metadata:
  labels:
    app.kubernetes.io/name: node-exporter
  name: node-exporter
  namespace: monitoring-system
spec:
  selector:
    matchLabels:
      app.kubernetes.io/name: node-exporter
  template:
    metadata:
      labels:
        app.kubernetes.io/name: node-exporter
    spec:
      containers:
        - name: node-exporter
          image: prom/node-exporter
          args:
            - --path.sysfs=/host/sys
            - --path.rootfs=/host/root
          ports:
            - containerPort: 9100
              name: prometheus
              protocol: TCP
          volumeMounts:
            - name: sys
              mountPath: /host/sys
              mountPropagation: HostToContainer
              readOnly: true
            - name: root
              mountPath: /host/root
              mountPropagation: HostToContainer
              readOnly: true
      volumes:
        - name: sys
          hostPath:
            path: /sys
        - name: root
          hostPath:
            path: /
```

The page I linked above has a few extras: it ignores various mount points and networking interfaces. I probably should
have done that; I didn't.

Once that's running, you need to decide how to export the metrics so that VictoriaMetrics can find them.

I initially tried this with a `VMNodeScrape` object, but it didn't work. It took me a little while to figure it out and
internalise it. The VictoriaMetrics scrapers define _discovery_: `VMNodeScrape` enumerates nodes, `VMPodScrape`
enumerates pods, and so on. You still need to ensure that the metrics port is accessible to the agent.

So, for example, if you're using a `VMNodeScrape`, that port must be exposed _on the node's IP address_. To make this
work, you'll need a `NodePort` service.

Or you can use a `VMPodScrape` or a `VMServiceScrape` (with a headless service). I went with a `VMPodScrape`:

```yaml
apiVersion: operator.victoriametrics.com/v1beta1
kind: VMPodScrape
metadata:
  name: node-exporter
  namespace: monitoring-system
spec:
  selector:
    matchLabels:
      app.kubernetes.io/name: node-exporter
  podMetricsEndpoints:
    - port: prometheus
      path: /metrics
      relabelConfigs:
        - sourceLabels:
            - "__meta_kubernetes_pod_node_name"
          action: replace
          targetLabel: instance
```

The only really interesting thing here is the `relabelConfigs`, which sets `instance` to the node name, rather than the
default `10.42.0.68:9100` (for example).

And now I can view CPU temperatures, load average, disk space, etc., in a grafana dashboard.
