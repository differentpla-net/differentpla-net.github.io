---
title: "EUnit examples: a simple test"
short_title: "A simple test"
date: 2023-03-31T08:48:00.000Z
tags: erlang eunit
layout: series
series: erlang-eunit-examples
---

A simple EUnit test might look like this:

```erlang
% in test/meaning_test.erl
-module(meaning_test).
-include_lib("eunit/include/eunit.hrl").

the_test() ->
    ?assertEqual(42, meaning:meaning_of([life, universe, everything])).
```

Of note:
- The module is in the `test/` directory.
- It's named with `_test.erl`.
- It includes (using `include_lib`) the `eunit.hrl` include file.
- Our test is named with `_test`.

The test can be run with `rebar3 eunit` or -- if you're using erlang.mk -- `make eunit`. If you're using `mix` to build
your Erlang project, then check out my [`mix_eunit`](https://github.com/rlipscombe/mix_eunit) project.
