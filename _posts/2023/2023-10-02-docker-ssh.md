---
title: "SSH Forwarding in Docker"
date: 2023-10-02T09:38:00.000Z
tags: docker
---

If you're building your application inside a Docker container, and you need to pull dependencies from a git server (e.g.
Github or Gitlab) using SSH, you need to forward your SSH session to the container.

Here's how I did it in Docker 24.0.

When you run `docker build`, tell it to expose your SSH keys to the build with the following:

```sh
docker build --ssh default .
```

Inside your `Dockerfile`, use `RUN --mount=type=ssh`. So, for example, if you're using NPM, that would be something like
the following:

```sh
RUN --mount=type=ssh npm install
```

## Restricting SSH key use

Try to restrict the scope of the SSH keys to just the step that fetches your dependencies.

For example, if you're using Erlang's rebar3, that would look like this:

```dockerfile
RUN --mount=type=ssh rebar3 get-deps
RUN rebar3 compile --deps_only
```

## Host key verification failed

When fetching from (e.g.) Github, you might see the following error:

```
Host key verification failed.
fatal: Could not read from remote repository.
```

To fix this, put the following in your `Dockerfile`:

```dockerfile
RUN mkdir -p -m 0700 $HOME/.ssh && \
    ssh-keyscan github.com >> $HOME/.ssh/known_hosts && \
    ssh-keyscan gitlab.com >> $HOME/.ssh/known_hosts
```

The above has Github and Gitlab; delete as appropriate.

## MitM

Note that you're potentially vulnerable to a MitM attack, because this blindly accepts the keys. If you're concerned
about that, read [this Server Fault
question](https://serverfault.com/questions/856194/securely-add-a-host-e-g-github-to-the-ssh-known-hosts-file).

## References

These are a bit dated, but they were useful in figuring out the details:

- [Build secrets and SSH forwarding in Docker 18.09](https://medium.com/@tonistiigi/build-secrets-and-ssh-forwarding-in-docker-18-09-ae8161d066)
- [Securely using SSH keys in Docker to access private Github repositories](https://www.fastruby.io/blog/docker/docker-ssh-keys.html)
- [Fix ‘Host key verification failed’ inside Dockerfile](https://medium.com/@marius.rad/fix-host-key-verification-failed-inside-dockerfile-a906cd77066b)
