---
title: Installing Python with python-build
date: 2019-01-30 17:20
layout: series
series: direnv-tool-versions
tags: direnv python
---

Install `pyenv`. We're not going to use it as-is, but it includes the `python-build` script that we will use:

    PYENV_VERSION=2.0.1
    mkdir -p ~/.direnv/
    cd ~/.direnv/
    wget -O pyenv-${PYENV_VERSION}.tar.gz \
        https://github.com/pyenv/pyenv/archive/v${PYENV_VERSION}.tar.gz

    tar -xzvf pyenv-${PYENV_VERSION}.tar.gz
    rm pyenv
    ln -s pyenv-${PYENV_VERSION} pyenv

    mkdir -p ~/.direnv/bin/
    ln -sf ~/.direnv/pyenv/plugins/python-build/bin/python-build ~/.direnv/bin/

You might need to install some [prerequisites](https://github.com/pyenv/pyenv/wiki/common-build-problems) first.

tl;dr:

    sudo apt-get install -y make build-essential libssl-dev zlib1g-dev libbz2-dev \
        libreadline-dev libsqlite3-dev wget curl llvm libncurses5-dev libncursesw5-dev \
        xz-utils tk-dev libffi-dev liblzma-dev python-openssl git

Then install your chosen Python version:

    ~/.direnv/bin/python-build 3.9.5 ~/.pyenv/versions/3.9.5

To get a list of available Python definitions, use:

    ~/.direnv/bin/python-build --definitions

To update the list of Python definitions, you need to download and
install a newer copy of [pyenv](https://github.com/pyenv/pyenv). Just
follow the instructions above, changing the version number.
