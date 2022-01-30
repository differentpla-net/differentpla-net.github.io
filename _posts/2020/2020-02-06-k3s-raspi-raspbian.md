---
title: "k3s on Raspberry Pi: Installing Raspbian"
short_title: "Installing Raspbian"
date: 2020-02-06T16:54:00
layout: series
series: k3s
tags: k3s raspberry-pi
---

I downloaded [Raspbian Buster Lite](https://www.raspberrypi.org/downloads/raspbian/) from the official site and wrote it to the SD cards (using `dd`).

Then I configured the card for headless SSH access (`touch .../boot/ssh`).

Then, one at a time, for each Raspberry Pi, I put the SD card in each Raspberry Pi and turned it on. This allowed me to discover the IP address (by seeing the new IP address appear on my router).

Then I configured them:

```
ssh-copy-id pi@192.168.1.201    # or whatever IP
ssh pi@192.168.1.201
echo 'rpi201' | sudo tee /etc/hostname
sudo shutdown -r now
```

Repeat the above, changing the hostname, for each Raspberry Pi.

Then update and configure each of them:

```
ssh pi@rpi201       # or whatever

sudo apt-get update
sudo apt-get -y upgrade

sudo apt-get install raspi-config
sudo raspi-config
# ^^ set graphics memory split to 16M

sudo apt-get install vim-nox
sudo vi /boot/cmdline.txt
# ^^ add the following:
# cgroup_enable=cpuset cgroup_memory=1 cgroup_enable=memory
sudo shutdown -r now
```

To save myself a bunch of typing, I mostly did the above by using `tmux`, incidentally.

<div class="callout callout-info" markdown="span">
**Update:** I've since [replaced Raspbian with Ubuntu]({% post_url 2021/2021-12-20-raspi-ubuntu %}), for 64-bit goodness.
</div>
