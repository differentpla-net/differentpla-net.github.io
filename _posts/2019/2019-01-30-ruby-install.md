---
title: Installing Ruby with ruby-install
date: 2019-01-30 17:10
layout: series
series: direnv-tool-versions
tags: direnv ruby
---

Install `ruby-install`:

https://github.com/postmodern/ruby-install/archive/refs/tags/v0.8.3.tar.gz

    mkdir -p ~/.direnv/
    cd ~/.direnv/
    wget -O ruby-install-0.8.3.tar.gz \
        https://github.com/postmodern/ruby-install/archive/v0.8.3.tar.gz

    tar -xzvf ruby-install-0.8.3.tar.gz
    ln -sf ruby-install-0.8.3 ruby-install

Create `~/.direnv/bin/ruby-install`:

```
mkdir -p ~/.direnv/bin/
cat > ~/.direnv/bin/ruby-install <<'EOF'
#!/bin/sh
~/.direnv/ruby-install/bin/ruby-install \
    --no-install-deps --src-dir ~/.rubies/src --cleanup $@
EOF

chmod +x ~/.direnv/bin/ruby-install
```

The `--no-install-deps` above prevents `ruby-install` from attempting to, well, install dependencies. If you leave this off, it'll run a `sudo apt install` (or whatever) on your behalf, which leaves you with an annoying password prompt.

The list of dependencies is in the `dependencies.txt` file, so we can install them ourselves:

    sudo apt install $(
    grep '^apt:' \
        ~/.direnv/ruby-install/share/ruby-install/ruby/dependencies.txt | \
        cut -d: -f2- )

Then install your chosen Ruby version (e.g. 2.6.6):

    ~/.direnv/bin/ruby-install ruby 2.6.6

The above command will install Ruby 2.6.6 to `~/.rubies/ruby-2.6.6`, which is where `direnv` [will look for it]({% post_url 2019-01-30-ruby-direnv %}).

## Listing available Ruby versions

```
$ ~/.direnv/bin/ruby-install
Stable ruby versions:
  ruby:
    2.4.10
    2.5.8
    2.6.6
    2.7.1
  jruby:
    9.3.2.0
... etc.
```

## Updating available Ruby versions

```
$ ~/.direnv/bin/ruby-install -L
>>> Downloading latest ruby versions ...
>>> Downloading latest jruby versions ...
...etc.
```

_Edited 2021-12-04: Update to ruby-install 0.8.3; more current Ruby versions._
