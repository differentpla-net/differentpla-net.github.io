## kubernetes-dashboard

By default, `kubectl port-forward` only binds to local addresses. This, while secure, is kinda pointless. Here's how to get it to bind to the "any" address:

```
kubectl port-forward -n kubernetes-dashboard service/kubernetes-dashboard :443 --address=0.0.0.0,::
```

You'll get a certificate warning; so you should probably sort that out.
