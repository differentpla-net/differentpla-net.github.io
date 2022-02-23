Idea: I need an amd64 node in my cluster, because I want to run (e.g.) TeamCity, etc. But: I don't want pods running
there by default.

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

...and have that apply to all pods by default, and then you could set a per-namespace node selector. Or not, because it
looks like the `PodNodeSelector` _prevents_ you from using particular node selectors at admission time. I'm struggling
to understand the documentation.

On the other hand, we can use taints and tolerations. If the amd64 nodes are tainted, then pods will need a matching
toleration in order to run on that node. You probably also want a node affinity, so that nodes ONLY schedule on those
nodes. The docs suggest writing a custom admission controller for that, or using the `ExtendedResourceToleration`
admission controller.

Problem with this is that certain system-ish pods; I don't care where they run, such as traefik, longhorn, etc. I'm only
interested in limiting where _my_ testing pods run. So I _don't_ want to set tolerations on the special stuff. Does that
mean an admission controller is required?
