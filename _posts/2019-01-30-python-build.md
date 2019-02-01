---
title: Installing Python with python-build
date: 2019-01-30 14:53
---

Install `pyenv`. We're not going to use it as-is, but it includes the `python-build` script that we will use:

    mkdir -p ~/.direnv/
    cd ~/.direnv/
    wget -O pyenv-1.2.9.tar.gz \
        https://github.com/pyenv/pyenv/archive/v1.2.9.tar.gz

    tar -xzvf pyenv-1.2.9.tar.gz
    ln -s pyenv-1.2.9 pyenv

    mkdir -p ~/.direnv/bin/
    ln -s ~/.direnv/pyenv/plugins/python-build/bin/python-build ~/.direnv/bin/

Then install your chosen Python version:

    ~/.direnv/bin/python-build 2.7.15 ~/.pyenv/pythons/2.7.15
