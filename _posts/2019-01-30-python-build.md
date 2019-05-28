---
title: Installing Python with python-build
date: 2019-01-30 17:08
layout: series
series: direnv-tool-versions
---

Install `pyenv`. We're not going to use it as-is, but it includes the `python-build` script that we will use:

    mkdir -p ~/.direnv/
    cd ~/.direnv/
    wget -O pyenv-1.2.11.tar.gz \
        https://github.com/pyenv/pyenv/archive/v1.2.11.tar.gz

    tar -xzvf pyenv-1.2.11.tar.gz
    ln -s pyenv-1.2.11 pyenv

    mkdir -p ~/.direnv/bin/
    ln -s ~/.direnv/pyenv/plugins/python-build/bin/python-build ~/.direnv/bin/

Then install your chosen Python version:

    ~/.direnv/bin/python-build 2.7.15 ~/.pyenv/pythons/2.7.15

To get a list of available Python definitions, use:

    ~/.direnv/bin/python-build --definitions

To update the list of Python definitions, you need to download and install a newer copy of pyenv. Just follow the instructions above, changing the version number.
