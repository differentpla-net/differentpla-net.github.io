---
title: "EUnit examples: WIP"
short_title: "WIP"
date: 2023-03-31T09:47:00.000Z
tags: erlang eunit
layout: series
series: erlang-eunit-examples
---

```erlang
simple_setup_single_test_() ->
    % 'setup' can take a single test; doesn't need to be a list.
    {setup, fun suite_setup/0, fun suite_cleanup/1, ?_assert(true)}.
```

```erlang
simple_foreach_single_test_nope() ->
    % 'foreach' MUST have a list.
    {foreach, fun setup/0, fun cleanup/1, fun pass/0}.

foreach_result_test_nope() ->
    % 'foreach' MUST have a list.
    {foreach, fun setup/0, fun cleanup/1, fun(_X) ->
        [
            fun pass/0,
            fun() -> ?assert(true) end,
            ?_assert(true)]
    end}.
```

Some nesting, and a reasonable example of foreachx, here: https://fossies.org/linux/apache-couchdb/src/couch/test/eunit/couch_db_split_tests.erl

## gen_servers

- <https://lookonmyworks.co.uk/2015/01/25/testing-a-gen_server-with-eunit/>

Next: How to stop processes; exit(kill) ain't doing it for me. Do we need to wait? What about using a supervisor?

https://stackoverflow.com/questions/21138442/stopping-an-erlang-supervisor

```erlang
exit_and_wait(Pid, Reason) ->
    MRef = monitor(process, Pid),
    exit(Pid, Reason),
    receive
        {'DOWN', MRef, process, Pid, _Reason} ->
            ok
    end.
```

A note on naming: JUnit -- https://junit.org/junit5/docs/current/user-guide/#writing-tests-annotations -- uses before/after each/all, which might be better for the examples.

There are also a few things in that list, like repeated tests, etc., that might be worth translating to eunit.

TODO: You can also decorate tests with a description.

## `setup` allows a single test generator

The test in `setup` doesn't have to be a list; it can be a single generator:

```erlang
simple_setup_single_test_() ->
    % 'setup' can take a single test; doesn't need to be a list.
    {setup, fun suite_setup/0, fun suite_cleanup/1, ?_assert(true)}.
```


```erlang
nested_setup_foreach_test_() ->
    % You can nest 'setup' and 'foreach'.
    {setup, fun suite_setup/0, fun suite_cleanup/1,
        {foreach, fun setup/0, fun cleanup/1, [
            fun() -> ?assert(true) end,
            fun() -> ?assert(true) end
        ]}}.

%! This doesn't work
setup_result_test_doesnt_work() ->
    % the result of Setup can be passed to each test, provided you wrap them as instantiators:
    {setup, fun suite_setup/0, fun suite_cleanup/1, [
        fun(_X) -> fun() -> ?assert(true) end end,
        fun(_X) -> ?_assert(true) end
    ]}.

setup_result_test_() ->
    % the result of Setup can be passed to each test, provided you wrap them as instantiators:
    {setup, fun suite_setup/0, fun suite_cleanup/1, fun(_X) -> [
        fun() -> ?assert(true) end,
        ?_assert(true)
    ] end}.

foreach_result_test_() ->
    {foreach, fun setup/0, fun cleanup/1, fun(_X) -> [
        fun() -> ?assert(true) end,
        ?_assert(true)
    ] end}.
```


TODO: you can also nest setup/setup and foreach/foreach.


Another alternative is to add a description to each test. You could use a macro:

```erlang
% TODO: Try to remember how we did this previously.
-define(test1(T), {T, fun() -> T()}

setup_result_test_() ->
    % the result of Setup can be passed to each test, provided you wrap them in an instantiator:
    {setup, fun suite_setup/0, fun suite_cleanup/1, fun(Pid) ->
        [
            ?test1(something),
            ?test1(another_thing)
        ]
    end}.

something_(_Pid) -> fun() -> ?assert(true) end.
another_thing_(_Pid) -> fun() -> ?assert(true) end.
```



Unfortunately, `setup` only takes a list of tests or a single instantiator, which means that the following doesn't work:

```erlang
setup_result_test_nope() ->
    % It doesn't want a list of instantiators, just an instantiator.
    {setup, fun suite_setup/0, fun suite_cleanup/1, [
        fun(_X) -> fun() -> ?assert(true) end end,
        fun(_X) -> ?_assert(true) end
    ]}.
```
