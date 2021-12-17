---
title: "Do docker registry names matter?"
date: 2021-12-17T21:29:00Z
tags: docker
---

When you want to push an image to a docker registry, you need to tag it with the registry name. Does the name matter?

**tl;dr:** no.

For example:

```
$ docker tag hello-world registry.example.com/hello-world
```

But if your registry is resolvable by different names (maybe it's also available as `docker.example.com`), does this
break things?

Having just [understood NodePort services]({% post_url 2021/2021-12-17-k3s-nodeport %}), I've got plenty of names I can
use to refer to my private docker repository. So let's find out.

```
% docker pull hello-world
% docker tag hello-world rpi401:30721/hello-world
% docker push rpi401:30721/hello-world
```

If I pull it from a different name, does that matter?

```
% docker pull rpi402:30721/hello-world
Using default tag: latest
latest: Pulling from hello-world
Digest: sha256:f54a58bc1aac5ea1a25d796ae155dc228b3f0e11d046ae276b39c4bf2f13d8c4
Status: Downloaded newer image for rpi402:30721/hello-world:latest
rpi402:30721/hello-world:latest
```

Apparently not, no. We end up with it listed multiple times locally:

```
% docker image ls | grep hello-world
hello-world                                                         latest                                                     feb5d9fea6a5   2 months ago    13.3kB
rpi401:30721/hello-world                                            latest                                                     feb5d9fea6a5   2 months ago    13.3kB
rpi402:30721/hello-world                                            latest                                                     feb5d9fea6a5   2 months ago    13.3kB
```

But they all have the same image ID, so I guess it doesn't particularly matter.
