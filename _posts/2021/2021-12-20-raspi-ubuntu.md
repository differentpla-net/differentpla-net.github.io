---
title: "Installing Ubuntu on Raspberry Pi"
date: 2021-12-20T12:17:00Z
layout: series
series: k3s
tags: raspberry-pi ubuntu
---

My [Raspberry Pi 4 cluster]({% post_url 2021/2021-07-04-k3s-raspi-updates %}) is currently 32-bit. It's got a 32-bit
kernel with a 32-bit userland. But I need to run 64-bit software on it. I looked into [upgrading it in place]({% post_url 2021/2021-12-20-upgrading-raspios-to-arm64 %}), but that's infeasible. So I need to reinstall it.

This time I'm going to use Ubuntu 21.10 rather than Raspbian / Raspi OS.

Since there's nothing of value on my cluster, I simply re-flashed the USB storage of each node with Ubuntu Server
by following [the instructions](https://ubuntu.com/tutorials/how-to-install-ubuntu-on-your-raspberry-pi#1-overview)
on the Ubuntu website.

I installed the nodes one at a time, because I'm using `ssh`, and it would be easy to mix the nodes up.

So, for each one:

1. Insert the USB stick and turn the node on.
2. Wait for a few minutes while the installer does its thing.
3. Log in and reset the password.
4. Change the hostname; reboot.
5. Copy my SSH public key.

## Log in; reset password

The installer uses the name `ubuntu`, so log into the node:
```
ssh ubuntu@192.168.28.155   # password is 'ubuntu'
```

It immediately prompts to change the password, so do that.

I generated a random password by using the following:

```
env LC_CTYPE=C tr -dc 'a-zA-Z0-9' < /dev/urandom | head -c 16 ; echo
```

I used the same password for each node. I'm going to use SSH with public keys, so I'll never need to enter it again.

## Change hostname; reboot

```
echo 'rpi405' | sudo tee /etc/hostname
sudo shutdown -r now
```

## Copy my SSH public key

```
ssh-copy-id ubuntu@rpi405
```

## ...and repeat

Then repeat, for each of the other nodes, one at a time.

## Upgrade them all

Finally, update all of them (`tmux` `synchronize-panes` is your friend here):

```
sudo apt update && sudo apt upgrade
sudo shutdown -r now    # new kernel; reboot
```

## 64-bit?

Yes:

```
ubuntu@ubuntu:~$ uname -a
Linux ubuntu 5.13.0-1008-raspi #9-Ubuntu SMP PREEMPT Wed Sep 29 08:27:44 UTC 2021 aarch64 aarch64 aarch64 GNU/Linux

ubuntu@ubuntu:~$ dpkg --print-architecture
arm64
```

## Install some basics

```bash
sudo apt-get install \
    --no-install-recommends \
    vim-nox silversearcher-ag jq gron
sudo update-alternatives --set editor /usr/bin/vim.nox

mkdir "$HOME/bin"
echo 'export PATH=$PATH:$HOME/bin' >> ~/.bashrc
```

Next: reinstall k3s.
