---
title: "EUnit examples: Passing the result from 'foreach'"
short_title: "Passing the result from 'foreach'"
date: 2023-04-02T11:23:00.000Z
tags: erlang eunit
layout: series
series: erlang-eunit-examples
---

Similar to passing the result from `{setup, Setup, ...` to each test, you can also pass the result from `{foreach,
Setup, ...` to each test, but there are some differences.

```erlang
foreach_result_test_() ->
    % 'foreach' MUST have a list, but it can be a list of instantiators.
    {foreach, fun setup/0, fun cleanup/1, [
        fun(_X) -> fun() -> pass() end end,
        fun(_X) -> fun() -> ?assert(true) end end,
        fun(_X) -> ?_assert(true) end
    ]}.
```

Unlike `setup`, where it accepts `Tests | Instantiator`, `foreach` requires a list: `[Test | Instantiator]`, so the
following doesn't work:

```erlang
foreach_result_test_nope() ->
    % 'foreach' MUST have a list. This fails.
    {foreach, fun setup/0, fun cleanup/1, fun(_X) ->
        [
            fun pass/0,
            fun() -> ?assert(true) end,
            ?_assert(true)]
    end}.
```

(see <https://www.erlang.org/doc/apps/eunit/chapter.html#Fixtures>)

But you can pass a list with a single instantiator which returns a list:

```erlang
foreach_result_single_instantiator_test_() ->
    % ...or a list with a single instantiator, which returns a list.
    {foreach, fun setup/0, fun cleanup/1, [fun(_X) ->
        [
            fun pass/0,
            fun() -> ?assert(true) end,
            ?_assert(true)]
    end]}.
```
