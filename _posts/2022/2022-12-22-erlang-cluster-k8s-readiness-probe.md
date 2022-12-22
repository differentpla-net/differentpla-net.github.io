---
title: "Erlang cluster on Kubernetes: Readiness Probe"
short_title: "Readiness Probe"
date: 2022-12-22T17:50:00.000Z
layout: series
series: erlang-cluster-k8s
tags: erlang kubernetes
---

While scaling up/down the deployment for my Erlang cluster, I regularly refresh the web page that displays cluster
members. Occasionally, I get a "502 Bad Gateway" error from Traefik. What's with that?

The issue is that the Ingress (or the Service, but it doesn't matter) starts sending traffic to the pod before it's
fully-started.

To fix this, I added a readiness probe:

```yaml
readinessProbe:
  httpGet:
    path: /readyz
    port: http
  initialDelaySeconds: 1
  periodSeconds: 10
  failureThreshold: 1
```

I've not put much thought into the delay/period settings; they seem to work mostly OK for now.

## Naming the endpoint

The use of `/readyz` is a common pattern in Kubernetes; it seems to come from Google, according to [this Stack Overflow
answer](https://stackoverflow.com/a/43381061/8446). The `z` suffix is to avoid collisions with actual endpoints
(`/status` vs `/statusz`).

I later found a blog post where [Gitlab uses a hyphen
prefix](https://about.gitlab.com/blog/2022/05/17/how-we-removed-all-502-errors-by-caring-about-pid-1-in-kubernetes/) to
avoid collisions, giving `/-/readiness` for example. That also seems like a reasonable convention.

I considered pointing it to the home page at `/`, but decided against that:

- `/readyz` is a convention, and conventions are usually a good thing.
- The home page at `/` might be expensive to generate, or might be quite large. Spamming it with health/readiness probes
  would be a bad thing. It would waste CPU and network bandwidth.
- We might want to add machine-readable diagnostics to the page. We can't do that if we use the home page.

## Endpoint visibility

However, having machine-readable diagnostics on an externally-facing route risks exposing information useful to a
hacker. For example: if we report the status of our backend databases, the names of those might be considered valuable.

You can restrict this with ingress rules, but that runs the risk of accidentally misconfiguring them.

Alternatively, you might decide to put the readiness/health probes on a different port. That makes it easier to restrict
access (just don't add a service for that port), but means slightly more complicated code, because you've got to run two
listeners.

It also means that you might end up in a situation where your readiness probe succeeds, but your home page doesn't.

This is particularly relevant when using Erlang, where -- because, ironically, it's so reliable -- you can easily end up
with one part of your application happily responding while another part is wedged.

Or you might up starting the handlers in the "wrong" order. Think carefully about your supervision tree; make sure the
readiness/health endpoints start _after_ your business endpoints and consider using `rest_for_one` so that a failure in
the business endpoint tears down the health endpoints as well.

For simplicity, I decided to ignore all of the above. `/readyz` it is.

## Cowboy

I added the handler to the cowboy router:

```erlang
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", home_handler, []},
            {"/readyz", readyz_handler, []}
        ]}
    ]),
```

It's implemented like this:

```erlang
-module(readyz_handler).
-export([init/2]).

init(Req0, Opts) ->
    Req = cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, <<"OK">>, Req0),
    {ok, Req, Opts}.
```

Even after fixing this, I still -- very occasionally -- see the same error when a request goes to a pod that's just been
terminated. I'll try to look into that later.
