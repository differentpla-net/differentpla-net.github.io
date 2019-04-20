---
title: Adding lager handlers at runtime
date: 2014-05-10T08:49:00Z
tags: erlang lager
---
You're using lager for logging in your Erlang program, and you discover that
your configuration isn't logging anything more verbose than `info`-level
messages. How do you bump that up to `debug`?

Like this, for `lager_file_backend`:

    Module = lager_file_backend,
    Path = "log/debug.log",
    Id = Path,
    Config = [{file, Path}, {level,debug}],

    supervisor:start_child(lager_handler_watcher_sup, [lager_event, {Module, Id}, Config]).

Or like this, for `lager_syslog_backend`, where the configuration is given differently:

    Module = lager_syslog_backend,
    Ident = "ident",
    Facility = local7,
    Id = {Ident, Facility},
    Config = [Ident, Facility, debug],

    supervisor:start_child(lager_handler_watcher_sup, [lager_event, {Module, Id}, Config]).

Note that the `{Module, Id}` term is usually [derived by
calling](https://github.com/basho/lager/blob/master/src/lager_app.erl#L154-165)
`Module:config_to_id(Config)`, but that's not a required export, which makes
things tricky.

**Updated 2014-11-28:** This page used to document adding the event handler
directly, but given what I've learned about [supervised event
handlers](http://blog.differentpla.net/blog/2014/11/07/erlang-sup-event/), it
now shows the correct way to do it.
