---
title: Installing Ruby with ruby-install
date: 2019-01-30 17:40
layout: series
series: direnv-tool-versions
tags: direnv ruby
---

Install `ruby-install`:

https://github.com/postmodern/ruby-install/archive/refs/tags/v0.8.3.tar.gz

    mkdir -p ~/.direnv/
    cd ~/.direnv/
    wget -O ruby-install-0.8.5.tar.gz \
        https://github.com/postmodern/ruby-install/archive/v0.8.5.tar.gz

    tar -xzvf ruby-install-0.8.5.tar.gz
    ln -sf ruby-install-0.8.5 ruby-install

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

```sh
# Debian, Ubuntu, etc.
sudo apt install $(
grep '^apt:' \
    ~/.direnv/ruby-install/share/ruby-install/ruby/dependencies.txt | \
    cut -d: -f2- )
```

On a Mac with Homebrew, it's slightly different:

```sh
# macOS with Homebrew
grep '^brew:' \
    ~/.direnv/ruby-install/share/ruby-install/ruby/dependencies.txt | \
    cut -d: -f2- | xargs brew install

# per the Homebrew caveats from above
export LDFLAGS="-L/opt/homebrew/opt/openssl@1.1/lib"
export CPPFLAGS="-I/opt/homebrew/opt/openssl@1.1/include"
export LDFLAGS="-L/opt/homebrew/opt/libffi/lib"
export CPPFLAGS="-I/opt/homebrew/opt/libffi/include"
```

Then install your chosen Ruby version (e.g. 2.7.4):

```sh
~/.direnv/bin/ruby-install ruby 2.7.4
```

### Apple Silicon

If you're on a recent Mac with Apple Silicon (arm64), you need to add `--enable-shared`, per <https://www.rubyonmac.dev/how-to-install-ruby-on-macos-12-6-apple-silicon>:

```
~/.direnv/bin/ruby-install ruby 2.7.4 -- --enable-shared
```

## Installation location

The above commands will install Ruby 2.7.4 to `~/.rubies/ruby-2.7.4`, which is where `direnv` [will look for it]({% post_url 2019/2019-01-30-ruby-direnv %}).

## Listing available Ruby versions

```
$ ~/.direnv/bin/ruby-install
Stable ruby versions:
  ruby:
    2.6.10
    2.7.6
    3.0.4
    3.1.2
  jruby:
    9.3.9.0
... etc.
```

## Updating available Ruby versions

```
$ ~/.direnv/bin/ruby-install -L
>>> Downloading latest ruby versions ...
>>> Downloading latest jruby versions ...
...etc.
```

- _Edited 2021-12-04:_ Update to ruby-install 0.8.3; more current Ruby versions.
- _Edited 2022-11-07:_ Update to ruby-install 0.8.5; add macOS arm64; more current Ruby versions.
