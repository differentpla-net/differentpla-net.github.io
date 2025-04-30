---
title: "Running multi-architecture containers in Podman and Docker"
date: 2025-04-30T14:58Z
tags: docker podman containers
---

I noticed some differences between podman on WSL2 and podman on macOS recently. I spent some time investigating.

## Background

I was playing around with multi-arch builds in podman on my Windows laptop at the weekend, using WSL2 (running Ubuntu
24.04 LTS). I got everything working, but there were some annoyances. At the time, I put these down to problems with the
documentation and the various blog posts I'd been using.

Back at work on Monday, I continued playing with it, but this time on my Macbook M1 (running macOS), and there were
differences.

## Version differences

The system-provided podman package for Ubuntu 24.02 is 4.9.3, which is kinda old. The version on my Mac (installed with
Homebrew) is 5.4.2, which is significantly newer.

The obvious (but I'm gonna call it out anyway) other difference is that my Windows laptop (a Surface Book 3) has an
x86_64 (Intel Core i7) CPU, and my Macbook M1 has an aarch64 (Apple Silicon) CPU.

Also of note: on macOS, podman makes use of the the "podman machine VM", which is a Linux VM running in Apple's
Hypervisor. This is because containers are a Linux thing, so we need Linux in there somewhere.

I'm not entirely sure how podman works on WSL2, given that WSL2 itself uses a managed Linux VM, running each Linux
distribution as an isolated container.

## podman on Macbook M1 (arm64)

On my Mac (podman 5.4.2, with qemu-user-static installed), I can run the following commands successfully:

```
$ podman run --platform=linux/arm64 alpine:3.21 uname -m
aarch64
```

```
$ podman run --platform=linux/amd64 alpine:3.21 uname -m
x86_64
```

This is all as expected; what is kinda weird is that podman uses the last-downloaded alpine image, rather than the
native one:

```
$ podman run alpine:3.21 uname -m
WARNING: image platform (linux/amd64) does not match the expected platform (linux/arm64)
x86_64
```

## Enabling multi-arch support in podman on WSL2

To run the "other" architecture, podman makes use of qemu, so you need to make sure that's installed. If it isn't, you
get "Exec format error" when running containers for the wrong architecture.

On my Macbook, it seems to be installed by default. On WSL 2, you need to run the following:

```sh
sudo apt install qemu-user-static
sudo podman run --rm --privileged docker.io/multiarch/qemu-user-static --reset -p yes
```

However, it turns out that rebooting disables multi-arch support. To re-enable it, you need to run the second command
again:

```sh
sudo podman run --rm --privileged docker.io/multiarch/qemu-user-static --reset -p yes
```

I don't know how to make this persistent.

Aside: terminating and restarting the `ubuntu` distribution (with `wsl --terminate ubuntu`) does _not_ reset it;
shutting down and restarting the "WSL 2 lightweight utility virtual machine" (with `wsl --shutdown`) _does_ reset it.
Rebooting the whole laptop (to install updates, say) also shuts down the WSL2 VM.

Note: if you've got both podman and Docker Desktop installed, starting the Docker Engine (by starting up Docker Desktop)
seems to automatically apply the qemu changes. That is: podman containers for arm64 were failing with "Exec format
error"; I started Docker Desktop and then podman containers for arm64 started working.

## podman on WSL2 (amd64)

Assuming qemu is installed and enabled, it works as expected:

```
$ podman run --platform=linux/amd64 alpine:3.21 uname -m
x86_64
```

```
$ podman run --platform=linux/arm64 alpine:3.21 uname -m
aarch64
```

It also uses the last-downloaded image if you don't specify `--platform`. It's not particularly surprising that the
behaviour hasn't changed, given that the podman version installed in WSL2/Ubuntu 24.04 is older than that installed in
macOS.

## Docker Desktop (on Windows)

If I run the same commands with Docker Desktop on Windows, I get the same results. The "does not match" error is
slightly more verbose, though:

```
$ docker run --platform=linux/arm64 alpine:3.21 /bin/uname -m
aarch64
$ docker run --platform=linux/amd64 alpine:3.21 /bin/uname -m
x86_64
$ docker run alpine:3.21 /bin/uname -m
WARNING: The requested image's platform (linux/arm64/v8) does not match the detected host platform (linux/amd64) and no specific platform was requested
aarch64
```

## Docker Desktop (on Mac)

This works (almost) the same as Docker Desktop on Windows, but if you don't specify `--platform`, it runs the correct
architecture, so you don't get the warning:

```
$ docker run --platform=linux/arm64 alpine:3.21 /bin/uname -m
aarch64
$ docker run --platform=linux/amd64 alpine:3.21 /bin/uname -m
x86_64
$ docker run alpine:3.21 /bin/uname -m
aarch64
```

I think that this is because I've got "Use containerd for pulling and storing images" enabled on my Mac, which
"...enables native support for multi-platform images...". I don't have it enabled on my Windows laptop.
