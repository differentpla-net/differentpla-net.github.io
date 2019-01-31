---
title: Installing Ruby with ruby-install
date: 2019-01-30 14:53
---

Install `ruby-install`:

    mkdir -p ~/.direnv/
    cd ~/.direnv/
    wget -O ruby-install-0.7.0.tar.gz \
        https://github.com/postmodern/ruby-install/archive/v0.7.0.tar.gz

    tar -xzvf ruby-install-0.7.0.tar.gz
    ln -s ruby-install-0.7.0 ruby-install

Then install your chosen Ruby version:

    ~/.direnv/ruby-install/bin/ruby-install \
        --src-dir ~/.rubies/src \
        --cleanup \
        ruby 2.4.4

It will prompt you for your password before starting, so that it can ensure that all of Ruby's source dependencies are installed. You can use `--no-install-deps` to skip this.

The above command will install Ruby 2.4.4 to `~/.rubies/ruby-2.4.4`, which is where `direnv` [will look for it]({% post_url 2019-01-30-ruby-direnv %}).
