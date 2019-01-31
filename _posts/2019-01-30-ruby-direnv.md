---
title: Integrating direnv with ruby-install
date: 2019-01-30 14:53
---

To integrate `direnv` with `ruby-install` -- installation instructions [here]({% post_url 2019-01-30-ruby-install %}) -- add the following to `~/.direnvrc`:

```
use_ruby() {
    RUBY_VERSION="$1"

    RUBIES_DIR="$HOME/.rubies"
    RUBY_PREFIX="$RUBIES_DIR/ruby-$RUBY_VERSION"
    if [ -s "$RUBY_PREFIX" ]; then
        load_prefix "$RUBY_PREFIX"
    else
        tput setaf 1
        echo "Ruby $RUBY_VERSION not available; using default"
        echo "See http://blog.differentpla.net/blog/2019/01/30/ruby-install/"
        tput sgr0
    fi
}
```

Then add the following (e.g., for Ruby 2.4.4) to your project's `.envrc`:

```
use ruby 2.4.4
layout ruby
```