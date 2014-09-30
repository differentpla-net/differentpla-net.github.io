---
title: Adding lager handlers at runtime
date: 2014-05-10T08:49:00Z
---
You're using lager for logging in your Erlang program, and you discover that your configuration isn't logging anything more verbose than `info`-level messages. How do you bump that up to `debug`?

Like this:

    gen_event:add_handler(lager_event, {lager_file_backend, "/path/to/debug.log"},
        [{file, "/path/to/debug.log"}, {level,debug}]).
    lager:update_loglevel_config().
