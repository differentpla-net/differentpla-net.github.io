---
title: Installing Erlang with kerl
date: 2019-01-30 17:04
layout: series
series: direnv-tool-versions
tags: direnv erlang
---

At Electric Imp, on developer PCs, we manage our Erlang versions with [kerl](https://github.com/yrashk/kerl).

Assuming you have `kerl` available in your path (and you've installed the [prerequisites]({% post_url 2019/2019-01-30-erlang-build-prerequisites %})), installing a version of Erlang is simple:

    kerl update releases
    kerl build 21.2 21.2
    kerl install 21.2 $HOME/.kerl/erlangs/21.2

You can tweak the Erlang build by setting the `KERL_CONFIGURE_OPTIONS`
environment variable. We don't bother. If we did, we'd probably put a suffix on
the build name and install directory.

The installation directory (`~/.kerl/erlangs`) was chosen to be similar to
where kiex installs Elixir versions.

We prefer building from github, using a tag:

    kerl build git https://github.com/erlang/otp.git OTP-21.2.4 OTP-21.2.4
    kerl install OTP-21.2.4 $HOME/.kerl/erlangs/OTP-21.2.4

This gives us the option of installing a patched fork, which has been necessary
in the past.

Note that you can set (e.g.) `MAKEFLAGS=-j6 kerl build ...` which will make the
build quicker.

In this case, the build name and installation directory are named after the tag
(`OTP-21.2.4`), which makes it easier to tell which versions were built from
releases or from tags.

To activate the installation, you can do as kerl says:

    . $HOME/.kerl/erlangs/OTP-21.2.4/activate

...or you can use `direnv`...

If you're running short of disk space, you can delete old builds:

    kerl cleanup all

This doesn't delete the installed versions.
