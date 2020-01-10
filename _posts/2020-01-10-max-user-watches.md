---
title: "max_user_watches"
date: 2020-01-07T20:07:00.000Z
tags: ubuntu ubuntu-install
layout: series
series: ubuntu-install
---

> Visual Studio Code is unable to watch for file changes in this large workspace

VS Code documentation is [here](https://code.visualstudio.com/docs/setup/linux#_visual-studio-code-is-unable-to-watch-for-file-changes-in-this-large-workspace-error-enospc).

Rather than editing `/etc/sysctl.conf`, I added `/etc/sysctl.d/60-inotify.conf`,
with the following contents:

```
fs.inotify.max_user_watches = 524288
```

To reload it, you need to run `sudo sysctl --system`.
