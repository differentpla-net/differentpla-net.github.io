---
title: Moving to AWS
date: 2014-11-30 11:02:54
---

*This is part of my [quest to sort out my website]({% post_url 2014/2014-11-13-website-rebuild %}).
Moving to AWS.*

My website hosting has been a little sketchy over the years. I've hosted it on
the rack at [empeg](http://www.empeg.com/); I've hosted it at home, on my DSL
line; I've hosted it on a VPS; I've hosted it in AWS; I've moved it to a VM
back at home; I've used Jekyll on GitHub Pages.

While github pages is certainly convenient, I want more functionality:

 - I want some Jekyll plugins.
 - I want to track 404 errors so that I can fix them.
 - I want to install (probably) [discourse](http://www.discourse.org/) for
 comments.
 - At some point, I might want to ditch Jekyll and go back to running my own
 (or another) blog engine.

On the other hand, I don't really want to host it at home again, because --
while my Internet connection is pretty-much rock solid -- I'll always have to
be aware that it's running on a VM on my main desktop PC, and I can't be
bothered with that.

So: back to AWS. I've opted for a **t2.micro** in US East.
