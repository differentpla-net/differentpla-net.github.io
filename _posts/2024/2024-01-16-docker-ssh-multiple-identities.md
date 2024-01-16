---
title: "Using the correct identity when SSH forwarding in Docker"
date: 2024-01-16T04:16:00.000Z
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
    --secret id=ssh,src=$(HOME)/.ssh/id_other \
    .
```

Then we need to edit the `Dockerfile` to tell SSH that we want to use that identity:

```dockerfile
# tell git which identity we want to use
RUN git config --global core.sshcommand "ssh -i /run/secrets/ssh -o IdentitiesOnly=yes"

# mount the ssh-agent *and* the private key secret, then run 'npm install' (or whatever)
RUN --mount=type=ssh \
    --mount=type=secret,id=ssh \
    npm install
```

Figuring out to to make this work for people (or CI pipelines) who _aren't_ using multiple identities is left as an
exercise for the reader.