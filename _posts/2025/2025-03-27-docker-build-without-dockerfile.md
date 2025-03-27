---
title: "docker build without a Dockerfile"
date: 2025-03-27T17:21Z
tags: docker
---

I recently wanted to create an Ubuntu container image with some extra stuff installed, but I couldn't be bothered to
create a `Dockerfile`. It turns out that docker has a `docker container commit` command, so I used that. Here's how.

First we use `docker run` with an `ubuntu` image, telling it to use `apt-get` to install our extra stuff. We use
`--cidfile` which causes the container ID to be written to the specified file. Then use use that container ID to commit
an image.

```sh
docker run --cidfile ubuntu-tmp.cid ubuntu \
    /bin/bash -c "export DEBIAN_FRONTEND=noninteractive; \
                    apt-get update && \
                    apt-get -y upgrade && \
                    apt-get -y install curl jq"
docker container commit $(<ubuntu-tmp.cid) ubuntu-tmp
rm ubuntu-tmp.cid
```

Unfortunately, the command we passed above becomes the default command for the new image, so we need another step to get
rid of that:

```sh
docker run --cidfile ubuntu-extra.cid ubuntu-tmp \
    /bin/bash
docker container commit $(<ubuntu-extra.cid) ubuntu-extra
rm ubuntu-extra.cid
```

Now when we run it, it has our extra stuff installed:

```
% docker run -it ubuntu-extra
root@7b1fedeaf0ed:/# curl --version
curl 8.5.0 [etc.]
root@7b1fedeaf0ed:/# jq --version
jq-1.7
```

In hindsight, however, I'm gonna end up putting the snippet at the top in a shell script, aren't I? So I might as well
just use a `Dockerfile`. Still, this might be useful to someone.