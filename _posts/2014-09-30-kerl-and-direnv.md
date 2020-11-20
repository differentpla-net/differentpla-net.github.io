---
title: Using kerl with direnv
date: 2014-09-30
tags: kerl direnv
---
`kerl` allows you to easily build and install multiple Erlang/OTP releases.
It's kinda like `nvm` or `rvm`, but for Erlang. It doesn't do everything, and
that's what `direnv` is for. `direnv` allows you to run commands upon entering
or leaving a particular directory.

## Installing `direnv`

**direnv** requires **make** and **go** to be installed, so on Ubuntu, that's:

    sudo apt-get install make golang

On Mac OS X, that's:

    brew update ; brew install go

Then follow (more-or-less) the instructions at
https://github.com/zimbatm/direnv#install:

    git clone http://github.com/zimbatm/direnv
    cd direnv
    make
    sudo make install       # sudo is optional on OS X.

To track the current directory, **direnv** hooks your shell. On **bash**, it
hooks `$PROMPT_COMMAND`. To get this to work, add the following near the end of
your `.bashrc`:

    type -P direnv &>/dev/null && eval "$(direnv hook bash)"

This is slightly different from the installation instructions on the direnv
homepage, because my bash profile is shared between all of the machines I use,
and not all of them will have direnv installed.

## Installing `kerl`

See https://github.com/yrashk/kerl#downloading, but it's essentially this:

    curl -O https://raw.githubusercontent.com/spawngrid/kerl/master/kerl
    chmod +x kerl

Note that, if you put kerl in `$PATH` for your primary user account, it
probably won't be available when attempting to run it via `sudo`, so it's
probably easier to put it in (e.g.) `/usr/local/bin`:

    sudo mv kerl /usr/local/bin

## Using `kerl` to build and install Erlang

In this example, I'll show building R16B03-1, with the default settings. This
is the version used at Electric Imp at the time of writing.

    kerl list releases

    kerl build R16B03-1 r16b03-1
    kerl list builds

    sudo kerl install r16b03-1 /usr/local/erlang-R16B03-1
    kerl list installations

## Activating an Erlang version using `kerl`

    . /usr/local/erlang-R16B03-1/activate

## Combining `kerl` with `direnv`

What if your team doesn't use the same locations for installing Erlang versions
with **kerl**? Then you need the following in your `.envrc` file:

    ERLANG_VERSION=r16b03-1

    if has kerl; then
        ERLANG_INSTALLATION=$(kerl list installations | grep "^$ERLANG_VERSION " | cut -d' ' -f2)
        if [ -x "$ERLANG_INSTALLATION/activate" ] ; then
            echo "Using Erlang $ERLANG_VERSION (in $ERLANG_INSTALLATION) via kerl."
            . $ERLANG_INSTALLATION/activate
        else
            echo "Erlang $ERLANG_VERSION not available; using default."
        fi
    else
        echo "kerl not available; using default Erlang."
    fi
