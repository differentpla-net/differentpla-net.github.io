---
title: "Erlang: Code Generation"
date: 2023-04-20T13:02:00.000Z
tags: erlang
---

Sometimes you need to load some static data from a file. If you find yourself needing to do that a _lot_, it can make
sense to load it once and keep it in memory. Here's how to do it at compile-time, in Erlang.

Before I start, you're probably asking "But Roger, the operating system's gonna cache the file for us anyway. Why don't we just open the file and read it every time we need the data?".

And you're right, but that involves a bunch of syscalls, and sometimes even _that_ isn't fast enough. For historical
context, we originally did this when the Spectre and Meltdown vulnerabilities had just been discovered, and
userspace/kernel transitions suddenly got _really_ expensive.

It's also worth mentioning that Erlang didn't have `persistent_term` at the time.

Plus, it's just _neater_.

So: how do we do it?

The end goal is that we have an Erlang module, exposing a function. Calling that function returns the static data. In
this case, it's a chunk of binary data.

Aside: if you've got structured data, you could load and parse it at startup. Or you could convert it to Erlang terms and use file:consult. Or you could parse it and use term_to_binary and save that. Or you could (somehow, needs investigation) wedge the terms into the Erlang forms that you're going to feed to the compiler below.

This is one way to do it: https://stackoverflow.com/questions/2160660/how-to-compile-erlang-code-loaded-into-a-string/2160696#2160696

Or, the way that I did it in the end:

Create a file named `embedded.erl`:

```erlang
-module(embedded).
-export([bin/0]).

bin() -> <<"Hello">>.
```

Compile it with erlc +to_pp, which is from here: http://erlang.org/pipermail/erlang-questions/2017-April/092141.html, possibly.

Or maybe from here: https://marianoguerra.github.io/presentations/2020-erlug-elixir-flavoured-erlang/#16; did I go to that talk?

That gives you something like this:

```erlang
{attribute,1,file,{"embedded.erl",1}}.
{attribute,1,module,embedded}.
{attribute,2,export,[{bin,0}]}.
{function,4,bin,0,
    [{clause,4,[],[],
         [{bin,4,[{bin_element,4,{string,4,"Hello"},default,default}]}]}]}.
{eof,5}.
```

If we literally stick that in our code, we can run it through compile:forms. Unfortunately, that binary string seems to be an optimisation. When was it introduced? 'cos it breaks things.

Let's try a binary with some other numbers in it.

```erlang
{attribute,1,file,{"embedded.erl",1}}.
{attribute,1,module,embedded}.
{attribute,2,export,[{bin,0}]}.
{function,4,bin,0,
          [{clause,4,[],[],
                   [{bin,4,
                         [{bin_element,4,{integer,4,42},default,default},
                          {bin_element,4,{integer,4,53},default,default},
                          {bin_element,4,{integer,4,64},default,default},
                          {bin_element,4,{integer,4,75},default,default},
                          {bin_element,4,{integer,4,86},default,default}]}]}]}.
{eof,5}.
```

Better. Then we can compile that directly into a beam file. Job done.

Something about debug info and dialyzer goes here.

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
