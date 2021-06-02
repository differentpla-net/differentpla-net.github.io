---
title: "ErlangLS version?"
date: 2021-06-02T09:16:00Z
tags: erlang vscode
---

I recently had to report a bug against ErlangLS (the Erlang Language Server). Here's how I discovered the version number:

```
$ code --list-extensions --show-versions | grep erlang-ls
erlang-ls.erlang-ls@0.0.25
```

At the time of writing, this is the latest version; see https://marketplace.visualstudio.com/items?itemName=erlang-ls.erlang-ls.

```
$ cd ~/.vscode/extensions/erlang-ls.erlang-ls-0.0.25/
$ cd erlang_ls/_build/default/bin
$ ./erlang_ls --version
Version: 0.15.0
```

Also the latest, at the time of writing.
