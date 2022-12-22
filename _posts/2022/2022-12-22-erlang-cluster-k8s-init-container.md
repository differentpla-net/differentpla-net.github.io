---
title: "Erlang cluster on Kubernetes: Basic init container"
short_title: "Simple init container"
date: 2022-12-22T09:56:00.000Z
layout: series
series: erlang-cluster-k8s
tags: erlang kubernetes
published: false
---

As explained [here]({% post_url 2022/2022-11-13-cert-signing-options %}), I'm going to use an [init
container](https://kubernetes.io/docs/concepts/workloads/pods/init-containers/) to issue the pod certificates.

This seems like it would be less invasive and more generic than making changes to the main container. Consider the
situation where you wanted to use that container with something other than _cert-manager_ (Amazon KMS, for example). By
separating that concern into an init container, we make it easier to change our minds later.

This post talks about adding a simple "Hello World!" init container to the pod. I'll add the certificate stuff in a
later post.

The `Dockerfile` is as follows:

```dockerfile
FROM docker.io/alpine

WORKDIR /erlclu-init
COPY erlclu-init.sh erlclu-init.sh

ENTRYPOINT ["/erlclu-init/erlclu-init.sh"]
```

The `erlclu-init.sh` script is as follows:

```sh
#!/bin/sh

echo "Hello from $(hostname)"
```

We add it to the deployment as follows:

```yaml
#...
spec:
  initContainers:
    - name: erlclu-init
      image: docker.k3s.differentpla.net/erlclu-init:0.1.0
      imagePullPolicy: Always
  containers:
    #...
```

The Makefile is as follows:

```makefile
all: build-init-image push-init-image

build-init-image:
        podman build -f Dockerfile.init -t erlclu-init

push-init-image:
        podman push erlclu-init $(DOCKER_REGISTRY)/erlclu-init:$(RELEASE_VSN)
```

Note: it expects to inherit the environment variables from the top-level Makefile.
