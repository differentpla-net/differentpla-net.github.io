---
title: Using direnv to manage tool versions
date: 2019-01-30 17:00
layout: series
series: direnv-tool-versions
tags: direnv
---

At Electric Imp, we use a [variety of different languages]({% post_url 2019/2019-01-30-electricimp-languages %}), and each project might be using a different mix of versions.

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

{% include _series_toc.html %}

## Resources

My direnv config is here: [https://github.com/rlipscombe/direnv-tools](https://github.com/rlipscombe/direnv-tools)
