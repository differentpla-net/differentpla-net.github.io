---
title: "RBAC notes"
date: 2022-01-07T18:50:00Z
tags: kubernetes
---

`role.yaml`:

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: Role
metadata:
  namespace: default
  name: pod-reader
rules:
- apiGroups: [""] # "" indicates the core API group
  resources: ["pods"]
  verbs: ["get", "watch", "list"]
```

`service-account.yaml`:

```yaml
apiVersion: v1
kind: ServiceAccount
metadata:
  name: cluster-demo
  namespace: default
```

`role-binding.yaml`:

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: RoleBinding
metadata:
  name: read-pods
  namespace: default
subjects:
- kind: ServiceAccount
  name: cluster-demo
  apiGroup: ""
roleRef:
  kind: Role
  name: pod-reader
  apiGroup: ""
```

## Aside: `view` ClusterRole

```
$ kubectl get clusterroles view -o yaml
...
rules:
- apiGroups:
  - ""
  resources:
  - endpoints
  - pods
  ...
  verbs:
  - get
  - list
  - watch
...
```
