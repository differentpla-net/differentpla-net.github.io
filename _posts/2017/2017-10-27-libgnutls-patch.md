---
title: Updating libgnutls28 on Ubuntu trusty
date: 2017-10-27 09:10+0000
layout: series
series: libgnutls-bug
---

Previously, I [posted about a bug]({% post_url 2017/2017-10-27-libgnutls-bug %}) we'd fallen foul of in libgnutls28 on Ubuntu 14.04. There's a [patch](https://bugs.launchpad.net/ubuntu/+source/gnutls28/+bug/1722411).

Here's how to apply it.

## Get the source code

We'll need to [build the Ubuntu package from source](https://askubuntu.com/questions/28372/how-do-i-get-and-modify-the-source-code-of-packages-installed-through-apt-get).

```
$ apt-get source libgnutls28-dev
Reading package lists... Done
Building dependency tree
Reading state information... Done
Picking 'gnutls28' as source package instead of 'libgnutls28'
E: Unable to find a source package for gnutls28
```

Oh. We're [missing a `deb-src` entry](https://unix.stackexchange.com/questions/158395/apt-get-build-dep-is-unable-to-find-a-source-package) corresponding to the `deb` entry for the `gnutls` package.

However, in order to add the `deb-src` entry, we need to [find the `deb` entry](https://askubuntu.com/questions/8560/how-do-i-find-out-which-repository-a-package-comes-from) in the first place.

First: what version of the package are we using? Different versions could come from different repositories.

```
$ dpkg -s libgnutls28-dev
Package: libgnutls28-dev
...
Version: 3.2.11-2ubuntu1.1
...
```

Next: where did each version come from?

```
$ apt-cache showpkg libgnutls28-dev
Package: libgnutls28-dev
Versions:
3.2.11-2ubuntu1.1 (/var/lib/apt/lists/archive.ubuntu.com_ubuntu_dists_trusty-updates_universe_binary-amd64_Packages) (/var/lib/apt/lists/security.ubuntu.com_ubuntu_dists_trusty-security_universe_binary-amd64_Packages) (/var/lib/dpkg/status)
  ...

3.2.11-2ubuntu1 (/var/lib/apt/lists/archive.ubuntu.com_ubuntu_dists_trusty_universe_binary-amd64_Packages)
  ...

```

Next: where's the `deb` entry for that repository?

```
$ grep -R trusty-updates /etc/apt/sources.list /etc/apt/sources.list.d/
/etc/apt/sources.list.d/official-package-repositories.list:deb http://archive.ubuntu.com/ubuntu trusty-updates main restricted universe multiverse
```

Exercise for the reader: add the corresponding `deb-src` line to that file.

Let's try that again.

## Building from source

```
$ sudo apt-get build-dep libgnutls28-dev
```

```
$ apt-get source libgnutls28-dev  # no sudo!
$ cd gnutls28-3.2.11
$ dpkg-buildpackage -rfakeroot -uc -b
```

Well, that appeared to work. We now have a bunch of `.deb` files in the parent directory.

Time to apply the patch.

## Apply the patch

```
$ cd ..
$ wget https://bugs.launchpad.net/ubuntu/+source/gnutls28/+bug/1722411/+attachment/5039125/+files/gnutls28_3.2.11-2ubuntu1.1_lp1722411_v2.debdiff
```

**Important:** The first patch attached to the bug has a memory leak; the link above is to the fixed patch.


```
$ patch -p0 < gnutls28_3.2.11-2ubuntu1.1_lp1722411_v2.debdiff
```

```
$ cd gnutls28-3.2.11
$ dpkg-buildpackage -rfakeroot -uc -b
```

And _that_ appears to work. Another bunch of `.deb` files appears.

## Try it out

```
$ sudo dpkg -i ../*.deb
```

Then I recompiled and re-tested my MCVE. The request to `googleapis.com` succeeded.

## Next steps

1. Test the fix, locally, with an agent.
2. Install the fix on our "canary" server. Work with the customer to confirm that it's fixed for them, too.
3. Work with our DevOps team to get the patch installed on the production servers in the next production deploy.
