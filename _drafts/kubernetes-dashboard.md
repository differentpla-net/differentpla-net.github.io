## Kubernetes Dashboard

kube proxy would not expose to 0.0.0.0, had to use SSH port forward in the end.

TODO: What's the difference between port-forward and proxy?

## kubernetes-dashboard

By default, `kubectl port-forward` only binds to local addresses. This, while secure, is kinda pointless. Here's how to get it to bind to the "any" address:

```
kubectl port-forward -n kubernetes-dashboard service/kubernetes-dashboard :443 --address=0.0.0.0,::
```

You'll get a certificate warning; so you should probably sort that out.
- What's the difference between port-forward and proxy?
  - Particularly in the context of the dashboard.
