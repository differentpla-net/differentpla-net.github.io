---
title: "Erlang cluster on Kubernetes: Speeding up the container build"
short_title: "Speeding up the container build"
date: 2022-12-21T19:49:00.000Z
layout: series
series: erlang-cluster-k8s
tags: erlang kubernetes docker
---

I noticed that whenever I made any change to the application, it caused the ~~docker~~podman build to re-fetch and
recompile all of the dependencies. On the tiny laptop I was using at the time, this was taking several extra minutes for
every build.

I asked [on Hachyderm](https://hachyderm.io/@rogerlipscombe/109360585009455763) and got pointed to [this
Dockerfile](https://github.com/mbta/tablespoon/blob/main/Dockerfile#L14-L18). It's for Elixir, but I was able to do
something similar:

```dockerfile
# ...

# Fetch deps into a separate layer; should improve caching.
COPY rebar.config rebar.config
COPY rebar.lock rebar.lock
RUN rebar3 get-deps
RUN rebar3 compile --deps_only

# Copy the rest and compile it
COPY . .
RUN rebar3 as prod release

# ...
```

By only copying the `rebar.config` and `rebar.lock` files, we don't invalidate the caching unless those change. Then we
can use `rebar3 get-deps` and `rebar3 compile --deps_only` to fetch and build the dependencies. That allows the
dependencies to be cached.

Only _then_ do we copy the rest of the files and build the release.

Note: changing the version number in `rebar.config` invalidates the cache and causes dependencies to be fetched and
compiled again. I'll fix that later (when I look into automatically versioning everything).
