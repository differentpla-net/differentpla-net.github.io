---
title: Integrating direnv with ruby-install
date: 2019-01-30 17:45
layout: series
series: direnv-tool-versions
tags: direnv ruby
---

To integrate `direnv` with `ruby-install` -- installation instructions [here]({% post_url 2019/2019-01-30-ruby-install %}) -- add the following to `~/.direnvrc`:

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

bundle_check() {
    # This has to come after 'layout ruby'
    if [ -f Gemfile ]; then
        gem list -i '^bundler$' >/dev/null || \
            gem install --no-ri --no-rdoc bundler && \
            bundle check
    fi
}
```

Then add the following (e.g., for Ruby 2.4.4) to your project's `.envrc`:

```
use ruby 2.4.4
layout ruby
bundle_check
```

The `layout ruby` takes care of setting up a rough equivalent to Python's virtualenv, but for Ruby. The `bundle_check` calls that function from `~/.direnvrc`, and checks that your gems are up to date.
