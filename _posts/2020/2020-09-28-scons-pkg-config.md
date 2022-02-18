---
title: "Package python was not found in the pkg-config search path when using scons"
date: 2020-09-28T17:49:52Z
tags: python scons
---

After upgrading to Ubuntu 20.04 at the weekend (this is not strictly relevant),
one of my builds started failing with the following error:

```
Package python was not found in the pkg-config search path.
Perhaps you should add the directory containing `python.pc'
to the PKG_CONFIG_PATH environment variable
No package 'python' found
OSError: 'pkg-config --cflags --libs python' exited 1:
...
  File ".../SConscript", line 11:
    env.ParseConfig('pkg-config --cflags --libs python')
...
```

I couldn't find anything on Google, so having solved the problem, I figured I'd
write it up for posterity.

## tl;dr

**scons runs your build under a clean environment**

This means that if you're using virtualenv, pyenv or (as in my case) direnv to manage
your Python environment, you might need to ensure that your `PKG_CONFIG_PATH` environment
variable is set in your `SConstruct`:

```python
if 'PKG_CONFIG_PATH' in os.environ:
    env['ENV']['PKG_CONFIG_PATH'] = os.environ['PKG_CONFIG_PATH']
```

Note: The above hasn't been tested, since our SConstruct file is kinda complicated, and I had to do something correspondingly trickier, but it _ought_ to work.

See [https://scons.org/doc/3.1.2/HTML/scons-user/ch07s03.html](https://scons.org/doc/3.1.2/HTML/scons-user/ch07s03.html) for more information.
