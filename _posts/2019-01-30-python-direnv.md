```
use_python() {
    PYTHON_VERSION="$1"
    if [ -s "$HOME/.pyenv/pythons/$PYTHON_VERSION/bin" ]; then
        load_prefix "$HOME/.pyenv/pythons/$PYTHON_VERSION"
    else
        tput setaf 1
        echo "Python $PYTHON_VERSION not available; using default"
        tput sgr0
    fi
}
```

```
$ python -V
Python 2.7.15
```

```
python -c "import sys; print(':'.join(x for x in sys.path if x))"
```

If you want to use virtualenv, add `layout python` to `.envrc`. For example:

```
use python 2.7.15
layout python
```
