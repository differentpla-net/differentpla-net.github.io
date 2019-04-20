---
title: "Library module name 'foo_nif' does not match calling module 'erl_eval'"
date: 2014-10-09 13:43:00Z
tags: erlang nif
---

It turns out that you can't load Erlang NIF libraries from the shell.

If you try, you get the following error:

    {error,{bad_lib,"Library module name 'foo_nif' does not match calling module 'erl_eval'"}}

Instead, you have to put together an Erlang module, looking something like the
following, and load that.

    -module(foo_nif).
    -on_load(on_load/0).

    on_load() ->
        % Note that the ".so" suffix is assumed.
        Path = ?MODULE,
        ok = erlang:load_nif(Path, 0).
