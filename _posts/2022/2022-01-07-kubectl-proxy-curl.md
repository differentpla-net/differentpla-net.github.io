---
title: "kubectl proxy and curl"
date: 2022-01-07T10:23:00Z
tags: kubernetes
---

```
$ kubectl proxy
Starting to serve on 127.0.0.1:8001
```

Now we can use curl without needing any auth:

```
$ curl http://127.0.0.1:8001
{
  "paths": [
    "/.well-known/openid-configuration",
    "/api",
    "/api/v1",
...etc.
```
