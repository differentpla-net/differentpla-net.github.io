---
title: "Erlang: Code Generation"
date: 2023-04-20T13:02:00.000Z
tags: erlang
---

I want to dynamically generate some Erlang code. Compile time vs. runtime.

How does the compiler do it?

Erlang has a bunch of different ways to express code:

- Source code. Simplest to understand, tedious string handling.

- The AST / compiler forms.

https://github.com/rlipscombe/mix_embed_binaries

erl_parse, compile:forms.

parse_trans uses erl_parse. exprecs, etc.

- ASM (.S)
- Core (.core); undocumented; can change.

There's `cerl`, which does core things.
https://www.erlang.org/blog/core-erlang-by-example/
https://baha.github.io/intro-core-erlang-1/
