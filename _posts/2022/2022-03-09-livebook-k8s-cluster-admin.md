---
title: "Livebook as K8s Cluster Admin"
date: 2022-03-09T12:29:00Z
tags: kubernetes
layout: series
series: k3s
---

Because I like experimenting with Kubernetes from Elixir Livebook, I made the service account into a cluster admin.

{% capture message %}
**Warning:** The following grants *cluster-admin* privileges to anyone using your Livebook instance. That's probably a
bad idea.
{% endcapture %}
{% include alerts/danger.html content=message %}

## danger/cluster-admin.yaml

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRoleBinding
metadata:
  name: livebook:default:cluster-admin
roleRef:
  apiGroup: rbac.authorization.k8s.io
  kind: ClusterRole
  name: cluster-admin
subjects:
- kind: ServiceAccount
  name: default
  namespace: livebook
```

<div class="callout callout-danger" markdown="span">
I'm going to repeat the warning above: This grants *cluster-admin* privileges to anyone using your Livebook instance.
That's probably a bad idea.
</div>
