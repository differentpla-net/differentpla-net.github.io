---
title: "Controlling Docker from inside a container"
date: 2023-09-21T09:00:00.000Z
tags: docker
---

Context: you're running integration tests inside a docker container, you're running your integration dependencies in other containers and you'd like your test fixtures to be able to control the latter from the former.

Consider a failure test where you'd like to kill (e.g.) a redis instance and check that your code deals with this appropriately.

You're gonna need to break out of your fixture container and do docker-y things.

Docker API: Using curl to talk to the socket. That's interesting in itself.

Dockerfile shenanigans:

```
docker build --build-arg DOCKER_GID="$(shell getent group docker | cut -d: -f3)"
```

Note that this doesn't work the same with Docker Desktop on Mac:

```
% getent group docker
% getent group staff
staff:*:20:root
% groups
staff [...no docker]
% ls -l /var/run/docker.sock
lrwxr-xr-x  1 root  daemon  45  6 Sep 14:07 /var/run/docker.sock@ -> /Users/rogerlipscombe/.docker/run/docker.sock
% ls -l /Users/rogerlipscombe/.docker/run/docker.sock
srwxr-xr-x  1 rogerlipscombe  staff  0 18 Sep 09:50 /Users/rogerlipscombe/.docker/run/docker.sock=
```

Need to add the docker CLI to the container, if we're going to use it directly. This is worth an investigation and a bit more fleshing out, with examples.

```dockerfile
ARG DOCKER_GID=999
RUN groupadd -g $DOCKER_GID docker
RUN usermod -a -G docker $USER

USER $USER
```

```
--mount "type=bind,src=/var/run/docker.sock,dst=/var/run/docker.sock
```

Why not `dind`? Dunno. I've never really used it. Maybe I should...?

Asides: if you're running the tests outside the container (on the host), but you need to communicate back to it (Electric Imp had mock HTTP servers running in the test fixtures, for example), you might need to discover the host IP, from the containers' point of view.

This is a weird one, since it's because we injected running code into the EI agents, so it was dynamic. I'm not sure there's an analogous requirement in most cases.

Anyhow:

```sh
{% raw %}
RUNNER_IP=$(docker network inspect $DOCKER_NETWORK --format '{{range .IPAM.Config}}{{.Gateway}}{{end}}')
{% endraw %}
```

Or `$(shell ...)`, obviously.

While I'm thinking about CT in docker containers, and I am, we had some other crap that:

1. Pretty-printed the test results. That was nice. It was a CT hook.
2. Reported progress to the terminal title. That was useful for long-running suites. It was an event handler.
3. A validation CTH that checked for matching init_per_group/end_per_group and init_per_suite/end_per_suite. The docs call this out, but you get ugly errors, and sometimes weird failures.

TODO: Review erlang/doc/apps/common_test/config_file_chapter.html; it might be a good way to inject stuff into ?config, for flexibility (docker vs. not-docker, etc.) -- ct:get_config, ct:require. These are interesting.

TODO: Getting remote coverage stats. Needs some hoop-jumping.

TODO: Using gun to talk to the docker engine API.
