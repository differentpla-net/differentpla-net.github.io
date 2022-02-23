You can use a `PodNodeSelector` admission controller --
https://kubernetes.io/docs/reference/access-authn-authz/admission-controllers/#podnodeselector; this sets the default
node selector for pods in a given namespace, or for the cluster as a whole. This means that you could set a node
selector:

```yaml
...
  nodeSelector:
    kubernetes.io/arch: amd64
...
```

I'm not entirely sure how to install that (or any) admission controller on k3s. Nor, in fact, how to work out which ones
are enabled.

- <https://kubernetes.io/docs/reference/access-authn-authz/admission-controllers/>
- <https://kubernetes.io/docs/reference/access-authn-authz/extensible-admission-controllers/>
