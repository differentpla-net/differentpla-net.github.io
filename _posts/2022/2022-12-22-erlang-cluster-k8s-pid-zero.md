---
title: "Erlang cluster on Kubernetes: pid zero"
short_title: "pid zero"
date: 2022-12-22T18:13:00.000Z
layout: series
series: erlang-cluster-k8s
---

Erlang doesn't like running as pid 0 (usually the init process). I used `tini` to fix it.

<div class="callout callout-info" markdown="span">
Apparently, running as pid 0 was addressed in OTP-23.x, so this might not be necessary.
</div>


```dockerfile
# final stage of multi-stage build...
FROM docker.io/alpine

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++

# Use 'tini' to avoid Erlang running as PID 0.
RUN apk add --no-cache tini

#...

ENTRYPOINT ["/sbin/tini", "--"]
CMD ["/erlclu/bin/erlclu", "foreground"]
```
