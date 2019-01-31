---
title: Installing Python with python-build
date: 2019-01-30 14:53
---

Install `pyenv`. We're not going to use it as-is, but it includes the `python-build` script that we will use:

    PYENV_SHA=6309aaf2

    mkdir -p ~/.direnv/
    cd ~/.direnv/
    wget -O pyenv-6309aaf2.tar.gz \
        https://github.com/pyenv/pyenv/archive/6309aaf2.tar.gz

    tar -xzvf pyenv-6309aaf2.tar.gz
    ln -s pyenv-6309aaf2 pyenv

    ln -s ~/.direnv/pyenv/plugins/python-build/bin/python-build ~/.direnv/bin/

Then install your chosen Python version:

    ~/.direnv/bin/python-build 2.7.15 ~/.pyenv/pythons/2.7.15
