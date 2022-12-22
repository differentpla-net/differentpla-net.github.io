---
title: "Erlang cluster on Kubernetes: Non-root user"
short_title: "Non-root user"
date: 2022-12-22T18:08:00.000Z
layout: series
series: erlang-cluster-k8s
---

The Erlang daemon that I've been using to demonstrate Erlang distribution on K8s is running as the root user. I'm going
to fix that.

## Creating the user

In the `Dockerfile`, I made the following changes:

```dockerfile
#...
COPY --from=build /build/_build/prod/rel/erlclu /erlclu

RUN addgroup -g 10000 erlclu && adduser -u 10000 -D -G erlclu -h /erlclu erlclu
USER erlclu
#...
```

This runs the alpine `addgroup` and `adduser` commands to create a non-root user. I used uid and gid 10000.

The `USER erlclu` command is:
- _after_ the `COPY`, since we want the files owned by `root` (it prevents the application from overwriting its own files).
- _before_ the `ENTRYPOINT`, since we want to run as the new user.

## securityContext

I updated the deployment file to specify a `securityContext`:

```yaml
#...
spec:
  securityContext:
    runAsNonRoot: true
    runAsUser: 10000
    runAsGroup: 10000
  serviceAccountName: erlclu
  #...
```

## Fixing ownership

After making the previous changes, the daemon would no longer start. As part of the startup script, the environment
variables in `vm.args.src` are expanded to create `vm.args`. That fails, because it no longer has permission to create
the new file.

To fix that, I tweaked the Dockerfile as follows:

```dockerfile
#...
FROM docker.io/alpine
ARG RELEASE_VSN
#...
RUN addgroup -g 10000 erlclu && adduser -u 10000 -D -G erlclu -h /erlclu erlclu
# Allow the startup script to write to ../releases/<vsn>/vm.args, etc.
RUN chown -R erlclu.erlclu /erlclu/releases/${RELEASE_VSN}
USER erlclu
#...
```

That requires passing the `RELEASE_VSN` argument when building the container:

```makefile
build-image:
       podman build -f Dockerfile --build-arg RELEASE_VSN=$(RELEASE_VSN) -t erlclu
```

There are probably other things that could be tweaked. Let me know if I've missed anything serious. This'll do for now.
