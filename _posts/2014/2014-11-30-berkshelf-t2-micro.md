---
title: Installing berkshelf on AWS t2.micro
date: 2014-11-30 18:26:45
published: false
---

*This is part of my [quest to sort out my website]({% post_url 2014/2014-11-13-website-rebuild %}).
Installing berkshelf on a t2.micro.*

If you use `sudo gem install berkshelf` on an AWS **t2.micro**, you will run
out of memory. This is because `dep-selector-libgecode` (a `berkshelf`
dependency) [takes about 2GB of RAM while
compiling](https://github.com/opscode/dep-selector-libgecode#requirements).

In fact, on a **t2.small**, you're likely to run out of memory as well, or
you'll get bored waiting for it to finish compiling. To get around this, pick
one of the following options:

* Use an instance type with more RAM.
* [Install ChefDK](http://downloads.getchef.com/chef-dk/), instead of using
  gems. This includes a bunch of stuff that you might not want, but it is the
  simplest option.
* Use `berks vendor` on a beefier computer, and commit the cookbooks to your
  `chef-repo`.
* <del>Use a [system-installed
  libgecode](https://github.com/opscode/dep-selector-libgecode#using-a-system-gecode-instead)
  instead.</del> Unfortunately, `dep-selector-libgecode` requires Gecode
  version 3, and Ubuntu 14.04 provides version 4.
