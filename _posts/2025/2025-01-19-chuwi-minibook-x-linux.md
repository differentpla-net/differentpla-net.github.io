---
title: Installing Linux on the Chuwi Minibook X
date: 2025-01-19T12:09:00Z
tags: 
---

Inspired by [this Mastodon
post](https://wandering.shop/@cstross/113279670998603629) by Charlie Stross, I
recently bought a Chuwi Minibook X N100. I do love me a tiny laptop, and I miss
my Samsung NC10 Netbook. I decided to install Linux on it. Here's how that went.

I found [this page](https://github.com/sonnyp/linux-minibook-x), which seems to
show that almost everything just works.

Initially, I tried Ubuntu 22.04, because that's what I had to hand. The
integrated Wifi wasn't detected, which I put down to an older kernel. I also
remembered that I'm not a big fan of Ubuntu's default desktop manager (my work
laptop is a Macbook Pro M1, and my daily driver -- for now at least -- is
Windows with WSL2 on a Surface Book 3).

So, this morning, I decided to try out Kubuntu, so I copied 24.04 LTS to a USB
stick and installed that.

And, yeah, everything basically just works. As noted in the link above, the
screen is initially rotated, but that's easily fixed -- except for GRUB, but
I'll live with that.

The screen works, including touch; the trackpad works; the Wifi works; the
audio works; the webcam works.

There's a little bit of screen tearing when scrolling rapidly, but I can live
with that. I found a [post on
Reddit](https://www.reddit.com/r/Chuwi/comments/1714l7g/fedora_linux_on_minibook_x_n100/)
that implies that it can be fixed by using Wayland, so I'll play with that
later.

The keyboard (it's a US layout) is pretty good. It's a little firm for my
tastes, so it sometimes misses my delicate keypresses, but I'll just have to
get used to pressing harder.

The screen is tiny, and the default resolution is pretty high, and my eyes
aren't getting any younger, so I need to figure out KDE scaling, but that's not
a problem with the laptop.

I've been using it -- installing the basics, light web browsing, writing this
blog post (with neovim in the terminal) -- for about 2 hours on battery;
Kubuntu claims that there's about 4h30 left. That seems reasonable for my use
case, but you're not going to get a full day on the road with it.
