---
title: "Tailscale Subnet Router"
date: 2022-11-25T19:27:00Z
tags: tailscale
---

I've been looking for a VPN solution for my home network for a while now, and I recently started using Tailscale.

## Tailnet

It allows you to install it on a number of your devices and have all of those devices on the same network (a "tailnet"),
no matter where they are.

That works like this:

First I signed up for a Tailscale account, which was simple.

Then I installed the app on my Synology Diskstation, on my phone, and on the small laptop I usually take out with me.
That puts all of those devices on the same `100.x.y.z` network and allowed me to connect to the mentioned devices by IP
address.

Shortly after I created my account, Tailscale started offering "MagicDNS", which allows you to access the devies on your
tailnet by name, rather than by IP address. It's, well, magic, and it allowed me to connect to my NAS from my laptop
without needing to remember the IP address.

## Subnet Router

However, I didn't want to install Tailscale on all of my devices at home, but I still wanted to be able to access those devices
from other places.

Tailscale supports this with "subnet router", so I read [the documentation](https://tailscale.com/kb/1019/subnets/), got
slightly confused by the fact that the text and the diagram use different IP address ranges, and then proceeded to set
it up as follows:

1. On the Diskstation, run `sudo tailscale up --advertise-routes=192.168.28.0/24` (because my home network is on
   `192.168.28.x`).
2. In the admin UI, go to that device and enable subnet routing.

...and that's it. Everything started working. I could ping the lounge PC at home from my laptop while sitting in the
lounge at my mother's house.

## DNS

It's still not quite enough, though; it'd be nice if DNS worked. Tailscale supports "split horizon" DNS, so I set that
up as follows:

1. In the admin UI, go to the DNS tab.
2. Under the Nameservers section, click "Add nameserver" and select "Custom".
3. Enter `192.168.28.1` (the IP address of my router); turn on "Split DNS"; enter `differentpla.net`.

Done. I can browse to my K3s cluster at `home.k3s.differentpla.net`; I can browse to the media PC at
`emby.home.differentpla.net`.

Magic.
