---
title: "FROM --platform=$TARGETPLATFORM in Podman and Docker"
date: 2025-05-01T07:34Z
tags: docker podman containers
---

While building my [erlang-cluster](https://github.com/rlipscombe/erlang-cluster) demonstration project for
multi-architecture, using podman, I noticed some weirdness about using `FROM --platform=...`.

<div class="callout callout-info" markdown="span">
tl;dr: docker always uses the build platform for `FROM`; podman uses whatever you last pulled.
</div>

After various amounts of head-scratching, I discovered that -- to get the correct architecture built -- I needed to add
`--platform=...` to the `FROM` command in my `Dockerfile`:

```dockerfile
FROM --platform=$TARGETPLATFORM docker.io/erlang:27.3.3-alpine AS build
```

But when I later loaded this Dockerfile in VS Code, it complained that this was superfluous: `Setting platform to
predefined $TARGETPLATFORM in FROM is redundant as this is the default behavior`.

It turns out that it's a difference in behaviour between Podman and Docker.

As mentioned [in part 2]({% post_url 2025/2025-04-30-multiarch-containers-podman-docker-2 %}), podman uses whichever
platform image you pulled last, whereas docker uses the platform image for the target platform (which defaults to the
build platform).

I'll demonstrate.

Recall that our simple `Dockerfile` looks like this:

```dockerfile
FROM alpine:3.21

RUN uname -m

CMD ["uname", "-m"]
```

Here's podman without the `FROM --platform=...`

```sh
# pull the wrong image first, to demonstrate.
$ podman pull --platform=linux/arm64 alpine:3.21
...

# build the image
$ podman build -t uname .
...

$ podman run --rm --name uname -it uname
WARNING: image platform (linux/arm64/v8) does not match the expected platform (linux/amd64)
aarch64
```

This is on amd64 (my Windows laptop running WSL2); you can see that it used whichever platform image was pulled last.
But, as shown in the earlier post, Docker uses the current (build) platform for the base image. This is the difference.

If I change the `Dockerfile` to this...

```dockerfile
FROM --platform=$TARGETPLATFORM alpine:3.21

RUN uname -m

CMD ["uname", "-m"]
```

...then podman does the correct thing:


```sh
# pull the wrong image
$ podman pull --platform=linux/arm64 alpine:3.21
...

# build the image
$ podman build -t uname .
...

# it's correct this time.
$ podman run --rm --name uname -it uname
x86_64
```

Adding `FROM --platform=...` makes podman work like docker, but also explains why the VS Code extension for Docker
complains that it's superfluous -- because, to Docker, _it is_:

```sh
$ docker build -t uname .
...

 => WARN: RedundantTargetPlatform: Setting platform to predefined $TARGETPLATFORM in FROM is redundant as this is the default behavior (line 2)                                                                                                                     s

...

 1 warning found (use docker --debug to expand):
 - RedundantTargetPlatform: Setting platform to predefined $TARGETPLATFORM in FROM is redundant as this is the default behavior (line 2)
```

## Conclusion

For podman, _always_ specify `--platform` when building, either on the `FROM` directive, or with `podman build --platform=...`
