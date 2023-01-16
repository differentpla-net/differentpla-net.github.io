---
title: Integrating direnv with python 3
date: 2020-06-12T18:15
layout: series
series: direnv-tool-versions
tags: direnv python python-3
---

By now you should be using Python 3.x. You can integrate it with direnv (2.21 or newer)
by putting the following in your `.envrc`

```
layout pyenv 3.11.1
```

Note that `pyenv` needs to be in your PATH. If you installed `pyenv` by
following the instructions [from earlier]({% post_url 2019/2019-01-30-python-build %}),
then do this:

```
PATH_add $HOME/.direnv/pyenv/bin
layout pyenv 3.11.1
```

Note that this expects Python to be installed in `$HOME/.pyenv/versions`, so
the `python-build` instructions need to be changed as follows:

```
~/.direnv/bin/python-build 3.11.1 ~/.pyenv/versions/3.11.1
```

And you're done.
