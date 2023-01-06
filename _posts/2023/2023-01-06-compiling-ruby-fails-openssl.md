---
title: "Compiling Ruby 2.7.7 on Ubuntu 22.04"
date: 2023-01-06T20:24:00.000Z
layout: series
series: direnv-tool-versions
tags: direnv ruby
---

Compiling Ruby 2.7.7 fails with the following error about openssl:

```
*** Following extensions are not compiled:
dbm:
        Could not be configured. It will not be installed.
        Check ext/dbm/mkmf.log for more details.
openssl:
        Could not be configured. It will not be installed.
        /home/roger/.rubies/src/ruby-2.7.7/ext/openssl/extconf.rb:111: OpenSSL >= 1.0.1, < 3.0.0 or LibreSSL >= 2.5.0 is required
        Check ext/openssl/mkmf.log for more details.
*** Fix the problems, then remove these directories and try again if you want.
...
/home/roger/.rubies/src/ruby-2.7.7/lib/rubygems/core_ext/kernel_require.rb:83:in `require': cannot load such file -- openssl (LoadError)
make: *** [uncommon.mk:373: do-install-all] Error 1
```

The problem is that Ubuntu now ships with OpenSSL 3.0.2. All of the workarounds on the Internet have us downloading and
compiling our own OpenSSL version 1.x. Life's too short for that.

Fortunately, `ruby-build` has us covered.

Install it by following the instructions at <https://github.com/rbenv/ruby-build#install-manually-as-a-standalone-program>, except for the destination:

```
wget -O ruby-build-v20221225.tar.gz https://github.com/rbenv/ruby-build/archive/refs/tags/v20221225.tar.gz
tar xfz ruby-build-v20221225.tar.gz
PREFIX=$HOME/.direnv/ruby-build ./ruby-build-20221225/install.sh
```

Then use it to install the desired version of Ruby. It will install OpenSSL 1.1.1s as well.

```
~/.direnv/ruby-build/bin/ruby-build 2.7.7 ~/.rubies/ruby-2.7.7/
```

The destination here is the same one where [use ruby]({% post_url 2019/2019-01-30-ruby-direnv %}) will look for it.
