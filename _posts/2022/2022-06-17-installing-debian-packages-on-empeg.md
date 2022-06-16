---
title: "Installing Debian packages on the empeg"
date: 2022-06-17T06:53:00Z
tags: empeg
---

Sometimes you need to install a binary on your empeg. In my case, I wanted `strace`.

The empeg runs a _really_ stripped-down installation of Debian 2.2 (potato), so we can grab packages from the Debian
archive. The packages are usually pretty large (in 1999 terms), and none of the `dpkg` machinery exists on the player,
so it makes sense to unpack the files on a "proper" Linux box first, delete the bits we don't need, and then upload the
files to the player.

**Important:** Do all of this in a scratch directory: `mkdir tmp ; cd tmp`.

The first thing we need to do is figure out where the `strace` package is:

```sh
wget http://archive.debian.org/debian/dists/potato/main/binary-arm/Packages.gz
zcat Contents-arm.gz | less
```

Search for `Package: strace`. Then, just below that, look for `Filename:`. It looks like this:

```
Package: strace
...
Depends: libc6 (>= 2.1.2)
Filename: dists/potato/main/binary-arm/utils/strace_4.2-4.deb
```

We can download that file:

```sh
wget http://archive.debian.org/debian/dists/potato/main/binary-arm/utils/strace_4.2-4.deb
```

Now, because we can't just `dpkg -i foo.deb` on the empeg, we'll need to unpack the files using `ar`:

```
ar p strace_4.2-4.deb data.tar.gz > strace_4.2-4.tar.gz
```

Then we need to repackage just the bits we need (omitting the docs and manpages, for example):

```
tar xfz strace_4.2-4.tar.gz
tar cvfz strace_4.2-4-minimal.tar.gz usr/bin
```

The exact list of files you need will vary; this'll do for `strace`.

Then send the files to the empeg, as described [here]({% post_url 2002/2002-03-07-empeg-files %}).

It's worth pointing out, however, that `strace` slows down the process being traced. On the empeg, that means it's
_really_ slow.
