---
title: "Simple log colouring in Erlang"
date: 2023-01-21T20:40:00Z
tags: erlang
---

By default, the improved logger originally introduced in Erlang/OTP-21 doesn't support per-level colours. This is
something that I miss from [`lager`](https://github.com/erlang-lager/lager) and from Elixir's `Logger`. Here's a simple
way to implement something like `lager`.

To be clear, there are a couple of implementations of this already. I'm doing this mostly as an explainer, and because I
might build on this later.

To turn on the logger, you put the following in your config file:

```erlang
[
    {kernel, [
        {logger_level, info},
        {logger, [
            {handler, default, logger_std_h, #{}}
        ]}
    ]}
].
```

Formatting can be controlled by the [`template`](https://www.erlang.org/doc/man/logger_formatter.html#type-template)
option:

```erlang
[
    {kernel, [
        {logger_level, info},
        {logger, [
            {handler, default, logger_std_h, #{
                formatter =>
                  {logger_formatter, #{
                    template => TemplateGoesHere
                  }}
            }}
        ]}
    ]}
].
```

Here's something pretty close to lager's default formatting:

```erlang
%...
formatter =>
  {logger_formatter, #{
    legacy_header => false,
    single_line => true,
    template => [
      time, " ", "[", level, "]",
      {pid, [" ", pid, ""], ""},
      {mfa, [" ", mfa, ":", line], ""},
      ": ", msg, "\n"
    ]
  }}
```

But we can do better. We can add colours.

To do that, we're going to need our own formatter:

```erlang
%...
formatter =>
  {logger_color_formatter, #{
    legacy_header => false,
    single_line => true,
    template => [
      time, " ", color, "[", level, "]",
      {pid, [" ", pid, ""], ""},
      {mfa, [" ", mfa, ":", line], ""},
      ": ", msg, reset, "\n"
    ]
  }}
```

<div class="callout callout-info" markdown="span">
Yeah; I went with en-US spellings. I'm not sure how I feel about that.
</div>

Note that we've added a term, `color`, before the opening bracket of the level, and `reset` before the `"\n"`.

The default formatter only understands `level` and `msg`, which gives us two possible ways to implement this:

1. Add them to metadata, or
2. Implement a custom formatter. I opted for this one.

All that the custom formatter needs to do is replace `color` with the ANSI escape code for the relevant colour, and
`reset` with the ANSI reset code.

That would look like this:

```erlang
-module(logger_color_formatter).

-export([config_check/1, format/2]).

config_check(Config) ->
    logger_formatter:check_config(update_config(Config, debug)).

format(LogEvent = #{level := Level}, Config) ->
    Config2 = update_config(Config, Level),
    logger_formatter:format(LogEvent, Config2).

update_config(Config = #{template := Template}, Level) ->
    Config#{template => update_template(Template, Level)};
update_config(Config, _Level) ->
    Config.

update_template(Template, Level) ->
    lists:map(fun
            (color) -> default_color(Level);
            (reset) -> "\e[0m";
            (Item) -> Item
        end, Template).

default_color(debug) -> "\e[0;38m";
default_color(info) -> "\e[1;37m";
default_color(notice) -> "\e[1;36m";
default_color(warning) -> "\e[1;33m";
default_color(error) -> "\e[1;31m";
default_color(critical) -> "\e[1;35m";
default_color(alert) -> "\e[1;44m";
default_color(emergency) -> "\e[1;41m".
```

And that's basically it.

The source code is here: <https://github.com/rlipscombe/logger_color_formatter>
