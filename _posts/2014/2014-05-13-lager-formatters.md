---
title: lager formatters
date: 2014-05-13T12:49:00Z
tags: erlang lager
---
## Overview

If you're using [lager](https://github.com/basho/lager/) for your Erlang logging,
you've probably left the default formatter well alone, by doing something like
this in your configuration file:

    {lager_console_backend, [info, true]},

However, lager's built-in `lager_default_formatter` defines a mini-DSL that allows
you to customise the formatting. Let's take a look at the default format, and break
it down.

## Default Format

The default format is defined [in lager_default_formatter.erl](https://github.com/basho/lager/blob/master/src/lager_default_formatter.erl#L59) and looks like this:

    [date, " ", time, " ", color, "[", severity, "] ",
                {pid, ""},
                {module, [
                        {pid, ["@"], ""},
                        module,
                        {function, [":", function], ""},
                        {line, [":",line], ""}], ""},
                " ", message, EOL]

What we have here looks a bit like an Erlang `iolist`, where certain atoms (`date`, `time`, `pid`, etc.)
are replaced with values at runtime.

Actually, apart from a couple of specific atoms (`message`, `date`, `time`, `severity`
-- see [the source](https://github.com/basho/lager/blob/master/src/lager_default_formatter.erl#L80)),
these are simply looked up in the metadata associated with the call to `lager:log`.

You can set some of this metadata by using `lager:md/1`; some of it is set by
lager's parse transform when it sees a call to (e.g.) `lager:warning`.

## Default Values

The language also supports [default values](https://github.com/basho/lager/blob/master/src/lager_default_formatter.erl#L92),
for example:

    {pid, ""}

...states that the process ID should be output. If it's not defined, then output
an empty string instead.

## Conditionals

There's also support for [conditionals](https://github.com/basho/lager/blob/master/src/lager_default_formatter.erl#L95).
These are specified as `{Prop, Present, Absent}`. For example:

    {function, [":", function], ""}

...states that if `function` is present in the metadata, then output `[":", function]`;
otherwise output an empty string.

This is taken to an extreme in the `module` clause:

    {module, [
        {pid, ["@"], ""},
        module,
        {function, [":", function], ""},
        {line, [":",line], ""}], ""}

...which says: if module is not defined, output an empty string. If module _is_
defined, then output `@` (if pid is defined -- to terminate the pid we might already
have printed), followed by the module name, followed by the function and line number
(both with prefixes and defaults).

## Conclusion

You can probably do all of the custom log formatting without having to write a custom
backend or a custom formatter, just by using `lager_default_formatter` and its formatting DSL.

