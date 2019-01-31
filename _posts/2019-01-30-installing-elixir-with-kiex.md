---
title: Installing Elixir with kiex
date: 2019-01-30 17:43
---

At Electric Imp, we manage our Elixir versions with kiex.

Assuming you have `kiex` available in your path, installing a version of Elixir is simple:

    kiex install 1.8.1

This installs to `~/.kiex/elixirs`, and can be activated with the following:

    source $HOME/.kiex/elixirs/elixir-1.8.1.env

If you've got the `kiex` function defined in your shell (source `~/.kiex/scripts/kiex`), then you can use `kiex use 1.8.1`.
