---
title: "Running AMD64 images on Macbook M1 (ARM64)"
date: 2025-04-29T16:38Z
tags: podman
---

Here's how to run AMD64 images on your Macbook M1.

Podman, running on macOS, uses a "podman machine VM". This is a Linux VM that runs all of your podman containers.

## Ensure podman machine is running

Check that the podman machine VM is running with `podman list`. You should see something like the following:

```
$ podman machine list
NAME                     VM TYPE     CREATED        LAST UP            CPUS        MEMORY      DISK SIZE
podman-machine-default*  applehv     4 minutes ago  Currently running  4           2GiB        100GiB
```

If `LAST UP` is showing `Never`, start the VM with `podman machine start`:

```
% podman machine start
Starting machine "podman-machine-default"

[... rootless mode ...]
[... listening on ... system helper not installed ...]

Machine "podman-machine-default" started successfully
```

Because I'm _also_ running Docker Desktop on this machine, I ignored the stuff about the API endpoint and the system
helper (which are used for Docker API compatibility).

## Create podman machine

If it reports `VM does not exist`, as shown here...

```
$ podman machine start
Error: podman-machine-default: VM does not exist
```

...then you'll need to create it with `podman machine init`, like this:

```
$ podman machine init
Looking up Podman Machine image at quay.io/podman/machine-os:5.4 to create VM

[...]

To start your machine run:

	podman machine start

```

Then run `podman machine start`.

----

Everything below this point is a bit sketchy. Come back to it.



## Run a basic alpine container

```
$ podman run --name alpine-arm64 -d --platform=linux/arm64/v8 alpine:3.21 /bin/sleep 60m
...
```

```
$ podman exec -it alpine-arm64 uname -m
aarch64
```

```
erlbld/tmp [develop ?2] % podman run --name alpine-amd64 -d --platform=linux/amd64 alpine:3.21 /bin/sleep 60m
Resolved "alpine" as an alias (/etc/containers/registries.conf.d/000-shortnames.conf)
Trying to pull docker.io/library/alpine:3.21...
Getting image source signatures
Copying blob sha256:f18232174bc91741fdf3da96d85011092101a032a93a388b79e99e69c2d5c870
Copying config sha256:aded1e1a5b3705116fa0a92ba074a5e0b0031647d9c315983ccba2ee5428ec8b
Writing manifest to image destination
370076a9dad8aae33f8a830c6a0324b9d86426ffe6badc119d4aad34cc49307c
erlbld/tmp [develop ?2] % podman ps
CONTAINER ID  IMAGE                          COMMAND         CREATED             STATUS             PORTS       NAMES
75f48073a04c  docker.io/library/alpine:3.21  /bin/sleep 60m  About a minute ago  Up About a minute              alpine-arm64
370076a9dad8  docker.io/library/alpine:3.21  /bin/sleep 60m  2 seconds ago       Up 2 seconds                   alpine-amd64
erlbld/tmp [develop ?2] % podman ps
CONTAINER ID  IMAGE                          COMMAND         CREATED        STATUS        PORTS       NAMES
75f48073a04c  docker.io/library/alpine:3.21  /bin/sleep 60m  2 minutes ago  Up 2 minutes              alpine-arm64
370076a9dad8  docker.io/library/alpine:3.21  /bin/sleep 60m  6 seconds ago  Up 6 seconds              alpine-amd64
erlbld/tmp [develop ?2] % podman ps
CONTAINER ID  IMAGE                          COMMAND         CREATED        STATUS        PORTS       NAMES
75f48073a04c  docker.io/library/alpine:3.21  /bin/sleep 60m  2 minutes ago  Up 2 minutes              alpine-arm64
370076a9dad8  docker.io/library/alpine:3.21  /bin/sleep 60m  7 seconds ago  Up 7 seconds              alpine-amd64
erlbld/tmp [develop ?2] % podman ps
CONTAINER ID  IMAGE                          COMMAND         CREATED        STATUS        PORTS       NAMES
75f48073a04c  docker.io/library/alpine:3.21  /bin/sleep 60m  2 minutes ago  Up 2 minutes              alpine-arm64
370076a9dad8  docker.io/library/alpine:3.21  /bin/sleep 60m  7 seconds ago  Up 8 seconds              alpine-amd64
erlbld/tmp [develop ?2] % podman ps
CONTAINER ID  IMAGE                          COMMAND         CREATED        STATUS        PORTS       NAMES
75f48073a04c  docker.io/library/alpine:3.21  /bin/sleep 60m  2 minutes ago  Up 2 minutes              alpine-arm64
370076a9dad8  docker.io/library/alpine:3.21  /bin/sleep 60m  8 seconds ago  Up 8 seconds              alpine-amd64
erlbld/tmp [develop ?2] % podman ps
CONTAINER ID  IMAGE                          COMMAND         CREATED        STATUS        PORTS       NAMES
75f48073a04c  docker.io/library/alpine:3.21  /bin/sleep 60m  2 minutes ago  Up 2 minutes              alpine-arm64
370076a9dad8  docker.io/library/alpine:3.21  /bin/sleep 60m  8 seconds ago  Up 9 seconds              alpine-amd64
erlbld/tmp [develop ?2] % podman ps
CONTAINER ID  IMAGE                          COMMAND         CREATED        STATUS        PORTS       NAMES
75f48073a04c  docker.io/library/alpine:3.21  /bin/sleep 60m  2 minutes ago  Up 2 minutes              alpine-arm64
370076a9dad8  docker.io/library/alpine:3.21  /bin/sleep 60m  9 seconds ago  Up 9 seconds              alpine-amd64
erlbld/tmp [develop ?2] % podman exec -it alpine-amd64 /bin/sh
/ # uname -m
x86_64
/ #
```

Didn't need to install qemu...? It's already installed...?
