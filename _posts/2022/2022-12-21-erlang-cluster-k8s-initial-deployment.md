---
title: "Erlang cluster on Kubernetes: Initial Deployment"
short_title: "Initial Deployment"
date: 2022-12-21T19:45:00.000Z
layout: series
series: erlang-cluster-k8s
tags: erlang kubernetes
---

For simplicity's sake, I created a new application with `rebar3 new app name=erlclu`. I very soon regretted this
decision, because I actually needed a release, so I ran `rebar3 new release name=whoops` and manually merged the relevant
pieces together.

## Creating a new container image

### Dockerfile

That needs a `Dockerfile`; I'll start with a two-stage alpine-based image, as follows:

```dockerfile
FROM docker.io/erlang:25.1.2.0-alpine AS build

COPY / /build
WORKDIR /build

RUN rebar3 as prod release

#
FROM docker.io/alpine

RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs && \
    apk add --no-cache libstdc++

COPY --from=build /build/_build/prod/rel/erlclu /erlclu

EXPOSE 8080

ENTRYPOINT ["/erlclu/bin/erlclu", "foreground"]
```

It uses an `erlang:alpine` image as the base, and runs `rebar3 release` to build the release. That's then copied into a
(smaller) `alpine` image to be deployed. Erlang requires a few dependencies to be installed (the `apk add` commands).
`ENTRYPOINT` runs the release in the foreground.

### Makefile

I'm doing this on my Windows laptop, using WSL2 and I've got Docker Desktop installed. Because I can't figure out how to
get Docker Desktop to trust the private CA on my cluster's docker registry, I'm using `podman` instead.

That ends up looking like this:

```makefile
RELEASE_VSN ?= 0.1.0
DOCKER_REGISTRY ?= docker.k3s.differentpla.net

all: build-image push-image

release:
	rebar3 as prod release

build-image:
	podman build -f Dockerfile -t erlclu

push-image:
	podman push erlclu $(DOCKER_REGISTRY)/erlclu:$(RELEASE_VERSION)
```

Nothing particularly surprising here. There's an extra `release` target so that I can run it locally.

### deployment.yaml

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlclu
  namespace: erlclu
spec:
  replicas: 1
  selector:
    matchLabels:
      app: erlclu
  template:
    metadata:
      labels:
        app: erlclu
    spec:
      containers:
      - name: erlclu
        image: docker.k3s.differentpla.net/erlclu:0.1.0
        imagePullPolicy: Always
        resources:
          limits:
            memory: "128Mi"
            cpu: "500m"
        ports:
        - containerPort: 8080
```

The only things of note here are:

- I've set `imagePullPolicy: Always`, because I've not particularly thought about versioning the container image, so
  it's better for nodes to pull a fresh image every time.
- I've set some basic resource limits.

## Comments

If you look at the [initial-deployment tag](https://github.com/rlipscombe/erlang-cluster/tree/initial-deployment),
you'll see that I just stuck the `k8s` directory inside the `erlclu` directory.

This is probably a mistake: the K8s application will eventually have a bunch of other things in it (init containers,
cronjobs, etc.), and it would make more sense to have it at the top-level.
