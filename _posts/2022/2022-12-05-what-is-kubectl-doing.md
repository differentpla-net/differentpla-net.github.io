---
title: "What is kubectl doing?"
date: 2022-12-05T10:49:00Z
tags: kubernetes
---

You're using `kubectl` to do something; you want to do the same using the Kubernetes API (e.g. with `curl`). How do you
figure out what `kubectl` is doing?

Turn up logging verbosity to at least **6**:

```
% kubectl -v 6 get nodes
...
I1205 10:54:32.631012   20267 round_trippers.go:553] GET https://roger-nuc0:6443/api/v1/nodes?limit=500 200 OK in 51 milliseconds
...
```

...and there's the URL it's using.

What if we want to look for a non-default resource in a different namespace, and pass a label selector?

```
% kubectl -v 6 --namespace erlclu get certificaterequests -l app=erlclu
...
I1205 10:55:47.549457   20405 round_trippers.go:553] GET https://roger-nuc0:6443/apis/cert-manager.io/v1/namespaces/erlclu/certificaterequests?labelSelector=app%3Derlclu&limit=500 200 OK in 62 milliseconds
```

There ya go.
