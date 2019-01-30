---
title: Using direnv to manage tool versions
date: 2019-01-30 17:08
---

At Electric Imp, we use a variety of different languages, and each project might be using a different mix of versions.

Until recently, we've been using [kerl](https://github.com/kerl/kerl) to manage Erlang versions, [kiex](https://github.com/taylor/kiex) to manage
Elixir versions, [rvm](https://rvm.io/) to manage Ruby versions and [nvm](https://github.com/creationix/nvm) to manage node versions. We've also been using virtualenv to manage Python 2.x virtual environments. We don't deliberately manage Go versions (yet).

_Yes, we do use all of those languages -- and more -- at Electric Imp._

For experimentation, I also have Rust, Haskell and OCAML installed on my development PC. These are out of scope (for now).

But: kerl, kiex and nvm only manage the installation of different versions. They don't automatically switch to the correct version as you switch between projects.

For this, I've been using [direnv](https://direnv.net/).

But: direnv and rvm don't play well together.

Our operations -- #DevOps -- team recently switched from *rvm* to *rbenv*, but that only covers Ruby versions. So I decided to try *asdf* for managing versions for _everything_.

That ... didn't work out so well.

- [pip is broken](https://github.com/danhper/asdf-python/issues/49)
- [bundler can't install native extensions](https://github.com/asdf-vm/asdf-ruby/issues/92)
- Some of our scripts directly use `nvm use`, which complains about the node
  prefix being set. That is: both *asdf* and *nvm* want to manage the node version;
  they don't play well together.
- We have a shonky script that expects `erl` to be the real thing, not asdf's
  shim. That's our fault, but it makes team adoption harder.

So I'm going back to *direnv*, but with some knobs on.

This forms the first of a series of blog posts:
- [Installing direnv]({% post_url 2019-01-30-installing-direnv %})
- [Installing kerl]({% post_url 2019-01-30-installing-kerl %})
- [Installing Erlang with kerl]({% post_url 2019-01-30-installing-erlang-with-kerl %})
- [Integrating kerl with direnv]({% post_url 2019-01-30-integrating-kerl-direnv %})
- [Getting Erlang version]({% post_url 2019-01-30-erlang-version%})
- [Installing kiex]({% post_url 2019-01-30-installing-kiex %})
- [Installing Elixir with kiex]({% post_url 2019-01-30-installing-elixir-with-kiex %})
- [Integrating kiex with direnv]({% post_url 2019-01-30-integrating-kiex-direnv %})
- [Installing Python with python-build]({% post_url 2019-01-30-python-build %})
- [Managing Python versions with direnv]({% post_url 2019-01-30-python-direnv %})
- [Installing Ruby with ruby-install]({% post_url 2019-01-30-ruby-install %})
- [Managing Ruby versions with direnv]({% post_url 2019-01-30-ruby-direnv %})
- [Integrating nvm with direnv]({% post_url 2019-01-30-nvm-direnv %})

## Resources

My `.direnvrc` file is in my `dotfiles` repo: https://github.com/rlipscombe/dotfiles/blob/master/direnvrc
