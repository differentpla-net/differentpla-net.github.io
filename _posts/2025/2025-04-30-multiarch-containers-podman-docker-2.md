---
title: "Building multi-architecture containers in Podman and Docker"
date: 2025-04-30T17:15Z
tags: docker podman containers
---

When I was investigating how to build multi-architecture containers in Podman, I noticed some ... oddities. So I wrote
them down. I'll also compare podman's behaviour to docker's behaviour.

All of these tests were run on podman 4.9.3 on Ubuntu (WSL2) and podman 5.4.2 (macOS). Both Windows and macOS are
running Docker Desktop (4.40.0 and 4.41.0, which should make no appreciable difference).

I'll use this simple `Dockerfile` to demonstrate:

```dockerfile
FROM alpine:3.21

RUN uname -m

CMD ["uname", "-m"]
```

It prints out the machine architecture (e.g. `x86_64` or `aarch64`) when it's built, and again when it's run, like this:

```
$ podman build -t uname .
STEP 1/3: FROM alpine:3.21
STEP 2/3: RUN uname -m
x86_64
--> 8b0d260af991
STEP 3/3: CMD ["uname", "-m"]
COMMIT uname
--> ccbe08d255d6
Successfully tagged localhost/uname:latest
ccbe08d255d67bf87fbf613ec7084bcfd6cf1520d14b06646f3e8b351072f27c
```

```
$ podman run --rm --name uname -it uname
x86_64
```

## It depends on the base image

If you've pulled a different architecture for the base image (here `alpine`), you get _that_ architecture.

Here, the last-pulled image was for amd64, so we get an image built with that base.

```
$ podman pull --platform=linux/amd64 alpine:3.21
...

$ podman build -t uname .
...

$ podman run --rm --name uname -it uname
x86_64
```

Here, we pulled arm64 most-recently, so we get an image built with that base.

```
$ podman pull --platform=linux/arm64 alpine:3.21
...

$ podman build -t uname .
...

$ podman run --rm --name uname -it uname
WARNING: image platform (linux/arm64/v8) does not match the expected platform (linux/amd64)
aarch64
```

This is not good: you get a different image depending on what you did previously (which could have been days or weeks
ago).

## Using `podman build --platform` fixes that

I'm running podman on `x86_64` (Ubuntu on WSL2) for this example.

```sh
# pull the wrong arch
$ podman pull --platform=linux/arm64 alpine:3.21
...

# build with the correct arch
$ podman build --platform=linux/amd64 -t uname .
...

# note that it runs the correct arch
$ podman run --rm --name uname -it uname
x86_64
```

## Specifying `podman build --platform` multiple times breaks it again

```sh
# specify multiple platforms
$ podman build --platform=linux/amd64,linux/arm64 -t uname .
[linux/arm64] STEP 1/3: FROM alpine:3.21
...
[linux/arm64] COMMIT uname
...
[linux/amd64] STEP 1/3: FROM alpine:3.21
...
[linux/amd64] COMMIT uname
```

```sh
$ podman run --rm --name uname -it uname
x86_64
```

```sh
# exact same command; same computer; immediately afterwards
$ podman build --platform=linux/amd64,linux/arm64 -t uname .
...
```

```sh
# different architecture in the resulting container
$ podman run --rm --name uname -it uname
WARNING: image platform (linux/arm64/v8) does not match the expected platform (linux/amd64)
aarch64
```

It picks the resulting image architecture arbitrarily. This is why manifests exist. I'll address those later.

## What about Docker?

Docker always builds with the correct base architecture by default. Here's what it looks like on an amd64 platform:

```sh
# pull the wrong architecture
$ docker pull --platform=linux/arm64 alpine:3.21
...

# build the image
$ docker build -t uname .
...

# run the image; note that it's the correct platform
$ docker run --rm --name uname -it uname
x86_64
```

If you specify a platform, it uses that:

```sh
$ docker build --platform=linux/amd64 -t uname .
...

$ docker run --rm --name uname -it uname
x86_64

$ docker build --platform=linux/arm64 -t uname .
...

$ docker run --rm --name uname -it uname
WARNING: The requested image's platform (linux/arm64) does not match the detected host platform (linux/amd64/v4) and no specific platform was requested
aarch64
```

What about specifying multiple platforms?

```
$ docker build --platform=linux/amd64,linux/arm64 -t uname .
ERROR: Multi-platform build is not supported for the docker driver.
Switch to a different driver, or turn on the containerd image store, and try again.
Learn more at https://docs.docker.com/go/build-multi-platform/
```

On my Windows laptop, Docker Desktop is configured to use the default image store, which doesn't support multi-platform
images.

This is a good error message.

On my Mac, I enabled the containerd image store when I was messing around with Docker's Kubernetes-in-Docker (kind) support; let's see how that goes:

```sh
# the build succeeds:
$ docker build --platform=linux/amd64,linux/arm64 -t uname .
...

# running it without a platform picks the native platform:
$ docker run --rm --name uname -it uname
aarch64

# choosing the platform explicitly works without rebuilding the image:
$ docker run --platform=linux/arm64 --rm --name uname -it uname
aarch64

$ docker run --platform=linux/amd64 --rm --name uname -it uname
x86_64
```

So, in this case, Docker makes it easier to build multi-platform images. It manages the manifests for you (if you've got
the containerd image store enabled).
