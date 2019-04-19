---
title: Installing direnv
date: 2019-01-30 17:01
layout: series
series: direnv-tool-versions
---

From [direnv.net](https://direnv.net/):

> `direnv` is an environment switcher for the shell.

# Installing direnv

## Ubuntu

`direnv` is available as an [Ubuntu package][direnv-deb], but unless you're on a really recent Ubuntu, it'll be outdated.

Download the appropriate binary from the [releases page](https://github.com/direnv/direnv/releases), mark it executable, and stick it somewhere in your PATH.

For example:

    curl -qfsSL https://github.com/direnv/direnv/releases/download/v2.19.0/direnv.linux-amd64 -o direnv
    chmod +x direnv
    sudo mv direnv /usr/local/bin

## macOS

    brew install direnv

[direnv-deb]: https://packages.ubuntu.com/search?keywords=direnv&searchon=names&suite=all&section=all

# Hooking the shell

Then you need to [hook it into the shell](https://github.com/direnv/direnv#setup).

I only care about bash:

    # at the end of ~/.bashrc
    eval "$(direnv hook bash)"

This (for bash at least) uses `PROMPT_COMMAND` to ensure that the direnv hook gets run whenever the prompt is re-displayed.

_Note: this has the interesting side effect that you can't do `(cd .. ; ruby -v)` and expect it to work._

`direnv` appears to do the same (prompt hook) for fish, but it appears to use a pre-command hook in zsh.
