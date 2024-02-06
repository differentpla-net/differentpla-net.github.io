---
title: "Using the correct identity when SSH forwarding in Docker"
date: 2024-01-16T14:16:00.000Z
tags: docker
---

Back in October, I wrote a post explaining [how to do SSH forwarding]({% post_url 2023/2023-10-02-docker-ssh %}) with
`docker build`. In November, I wrote about [how to use multiple SSH identities with git]({% post_url
2023/2023-11-17-multiple-git-identities %}). Unfortunately, these don't always mix: using SSH in the docker build uses
the _first_ SSH identity held by `ssh-agent`, which might not be the one you wanted. Here's how I got around this
problem.

The answer is to tell SSH, running in docker build, which identity we want to use.

The first thing we need to do is make the private key available to `docker build`. We can use a secret for this:

```sh
docker build \
    --ssh default \
    --secret id=ssh_id,src=$(HOME)/.ssh/id_other \
    --build-arg GIT_SSH_COMMAND="ssh -i /run/secrets/ssh_id -o IdentitiesOnly=yes" \
    .
```

Then we need to edit the `Dockerfile` to tell SSH that we want to use that identity:

```dockerfile
# Set the GIT_SSH_COMMAND environment variable.
ARG GIT_SSH_COMMAND

# mount the ssh-agent *and* the private key secret, then run 'npm install' (or whatever)
RUN --mount=type=ssh \
    --mount=type=secret,id=ssh_id \
    npm install
```

To make this transparent for people (or CI pipelines) who _aren't_ using multiple identities, you can omit the
`--secret` and `--build-arg` options from the `docker build` command:

```sh
docker build \
    --ssh default \
    .
```

In my Makefile, that looks like this:

```makefile
# If you're using more than one SSH identity, set DOCKER_SSH_ID_SECRET to point to the ~/.ssh/id_whatever file.
ifdef DOCKER_SSH_ID_SECRET
_DOCKER_BUILD_SECRET_ARG = --secret id=ssh_id,src=$(DOCKER_SSH_ID_SECRET)
_DOCKER_BUILD_GIT_CONFIG_ARG = --build-arg GIT_SSH_COMMAND="ssh -i /run/secrets/ssh_id -o IdentitiesOnly=yes"
endif

docker-image:
	docker build \
		$(_DOCKER_BUILD_SECRET_ARG) \
		$(_DOCKER_BUILD_GIT_CONFIG_ARG) \
		--ssh default \
		.
```
