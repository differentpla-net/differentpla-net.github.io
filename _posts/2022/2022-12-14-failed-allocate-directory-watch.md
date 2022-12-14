---
title: "Failed to allocate directory watch"
date: 2022-12-14T10:00:00.000Z
tags: ubuntu ubuntu-install
---

While running `apt-get upgrade`, I was getting a `Failed to allocate directory watch: Too many open files` error.

It's similar to [this problem]({% post_url 2020/2020-01-10-max-user-watches %}).

You can see the current limits with `sysctl fs.inotify`. On Ubuntu, with the
defaults, these are as follows:

```
fs.inotify.max_queued_events = 16384
fs.inotify.max_user_instances = 128
fs.inotify.max_user_watches = 65536
```

To fix the problem, I added `/etc/sysctl.d/60-fs-inotify.conf`, with the
following contents:

```
fs.inotify.max_queued_events = 32768
fs.inotify.max_user_instances = 512
fs.inotify.max_user_watches = 524288
```

To reload the new settings, you need to run `sudo sysctl --system`.
