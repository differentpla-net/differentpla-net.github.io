---
title: "Installing rsh on the empeg"
date: 2002-03-07T12:21:00.000Z
tags: empeg
---

Grab a copy of rsh-client from the debian archive.

You'll need a line saying `root::0:0:root:/:/bin/sh` in `/etc/passwd` on the empeg.

You'll need a line saying `empeg-ip-address root` in `.rhosts` on your server.

```sh
rsh -l fred server-ip-address ls
```

...should work.
