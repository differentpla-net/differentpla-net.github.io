---
title: "Formatting Erlang code in Visual Studio Code"
date: 2023-01-15T17:04:00Z
tags: vscode erlang rebar3
---

The ErlangLS extension for VS Code includes formatting, using `rebar3_format`. I'd prefer to use `erlfmt`, so here's how it should be set up.

<div class="callout callout-info" markdown="span">
This is a corrected version of [this post]({% post_url 2022/2022-09-24-vscode-erlang-formatting %}).
</div>

## Install VS Code extension

- Install the `erlang-ls.erlang-ls` extension.

## Configure the formatter

If you want to do this globally, create `~/.config/rebar3/rebar.config` containing the following:

```erlang
{plugins, [rebar3_format, erlfmt]}.
{format, [
        {formatter, erlfmt_formatter}
]}.
```

<div class="callout callout-warning" markdown="span">
The `--files` option to `rebar3 format` wants to handle its own wildcards, so use quotes: `rebar3 format --files 'src/*.erl'`
</div>

## Links

- <https://github.com/AdRoll/rebar3_format#erlfmt>
- <https://github.com/whatsapp/erlfmt>
