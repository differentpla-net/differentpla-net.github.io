---
title: "Formatting Erlang code in Visual Studio Code"
date: 2022-09-24T13:58:00Z
tags: vscode erlang
---

The ErlangLS extension for VS Code includes formatting, using `rebar3_format`. I'd prefer to use `erlfmt`, so here's how I set it up.

These instructions are mostly lifted from <https://marketplace.visualstudio.com/items?itemName=szTheory.erlang-formatter>.

## Install rebar3

```sh
cd ~/projects
git clone https://github.com/erlang/rebar3.git
cd rebar3
./bootstrap
./rebar3 local install
```

```sh
export PATH=$PATH:~/.cache/rebar3/bin
```

## Choose default rebar3 formatter

```sh
echo -e "{plugins, [erlfmt]}." >> ~/.config/rebar3/rebar.config
```

## Install VS Code Extensions

- Install both `erlang-ls.erlang-ls` and `sztheory.erlang-formatter` extensions.

## Configure VS Code

Configure VS Code to use `sztheory.erlang-formatter`:

```json
"[erlang]": {
  "editor.defaultFormatter": "szTheory.erlang-formatter",
  "editor.formatOnSave": false
},
"erlangFormatter.formatter": "erlfmt",
```

Note that I turned off `formatOnSave` and chose `erlfmt` instead of
`rebar3_format`.

## Links

- <https://marketplace.visualstudio.com/items?itemName=szTheory.erlang-formatter>
- <https://github.com/whatsapp/erlfmt>