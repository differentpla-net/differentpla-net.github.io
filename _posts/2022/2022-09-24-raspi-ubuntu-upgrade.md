---
title: "Upgrading to Ubuntu 22.04 on my k3s cluster"
short_title: "Ubuntu 22.04"
date: 2022-09-24T13:58:00Z
layout: series
series: k3s
tags: k3s raspberry-pi
---

This afternoon, I fired up my k3s cluster for the first time in a while. When I ran `apt update`, I got an error message
about a missing Release file.

## Error Message

```
E: The repository 'http://ports.ubuntu.com/ubuntu-ports impish Release' no longer has a Release file.
```

## Cause

It turns out that [Ubuntu 21.10]({% post_url 2021/2021-12-20-raspi-ubuntu %}) is [no longer
supported](https://askubuntu.com/questions/1420077/21-10-removed-repositories), so I'm going to have to upgrade to 22.04
(which is LTS, so I won't need to do it again for a while).

## Upgrading

I'm going to run this on a "canary" node, to check it all works, then I'll just yolo it onto all of the others:

```
sudo do-release-upgrade
```

### Removing obsolete packages

When it asks about removing obsolete packages, _don't_, because one of those obsolete packages is the currently-running
kernel. It's going to continue to work, but it'll half-remove it, which will confuse `apt` later.

Instead: wait until after the reboot, and _then_:

```
sudo apt update
sudo apt upgrade
sudo apt autoremove
```

### Packages kept back

During the upgrade, several packages were "kept back". In Ubuntu 22.04, this could be because of "phased rollout". To check, run the following, specifying one of the kept-back packages:

```
$ apt-cache policy systemd
systemd:
  Installed: 249.11-0ubuntu3
  Candidate: 249.11-0ubuntu3.6
  Version table:
     249.11-0ubuntu3.6 500 (phased 0%)       <---- THIS
        500 http://ports.ubuntu.com/ubuntu-ports jammy-updates/main arm64 Packages
 *** 249.11-0ubuntu3 500
        500 http://ports.ubuntu.com/ubuntu-ports jammy/main arm64 Packages
        100 /var/lib/dpkg/status
```

If this is the case, then ignore the kept-back packages; they'll be updated at some point soon.

## Firmware backups

When I next restarted, I got the following message:

```
/boot/firmware is using 90.1% of 252MB
```

When upgrading Ubuntu, it saved a bunch of backups in the `/boot/firmware` directory. Provided the Raspberry Pi is working fine, these can be deleted:

```
sudo rm /boot/firmware/*.bak
```

Usage goes back down to about 46%.

## Upgrading k3s

I next upgraded k3s; see [this page]({% post_url 2022/2022-01-03-upgrading-k3s %}).
