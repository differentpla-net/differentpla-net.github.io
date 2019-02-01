---
title: Integrating direnv with python-build
date: 2019-01-30 14:53
---

To integrate `direnv` with `python-build` -- installation instructions [here]({% post_url 2019-01-30-python-build %}) -- add the following to `~/.direnvrc`:

```
use_python() {
    PYTHON_VERSION="$1"

    PYTHONS_DIR="$HOME/.pyenv/pythons"
    PYTHON_PREFIX="$PYTHONS_DIR/$PYTHON_VERSION"
    if [ -s "$PYTHON_PREFIX" ]; then
        load_prefix "$PYTHON_PREFIX"
    else
        tput setaf 1
        echo "Python $PYTHON_VERSION not available; using default"
        echo "See http://blog.differentpla.net/blog/2019/01/30/python-build/"
        tput sgr0
    fi
}
```

Then add (e.g.) the following to your project's `.envrc`:

```
use python 2.7.15
layout python
```

**Important:** Make sure you've installed `virtualenv`, _outside_ any virtualenv:

    $(/usr/bin/python -m site --user-base)/bin/pip install --user virtualenv

*The `$(...)` bit is to escape from your virtualenv, if you're already in one. ([h/t](https://github.com/sjml/dotfiles/blob/master/zsh.d.symlink/functions/pip))*

The `layout python` takes care of setting up a Python virtualenv for you.
