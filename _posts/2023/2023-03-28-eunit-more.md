---
title: "More eunit stuff"
date: 2023-03-28T08:47:00.000Z
tags: erlang eunit
---

Some examples of how to use Erlang's unit-testing framework, EUnit. Starting with simple examples and getting more
advanced.

A simple EUnit test might look like this:

```erlang
% in test/meaning_test.erl
-module(meaning_test).
-include_lib("eunit/include/eunit.hrl").

the_test() ->
    ?assertEqual(42, meaning:of([life, universe, everything])).
```

## `setup`

What if we want to run some setup before a set of tests (and cleanup afterwards)?

```erlang
suite_setup() ->
    % Setup for a group of tests goes here.
    % For example:
    {ok, Pid} = everything_server:start_link(),
    Pid.

suite_cleanup(Pid) ->
    % The result from suite_setup is passed as the argument to suite_cleanup.
    exit(Pid, kill),    % ...but see below.
    ok.

simple_setup_test_() ->
    % 'setup' sets up a single fixture for running all of the specified tests.
    {setup, fun suite_setup/0, fun suite_cleanup/1, [
        fun pass/0,
        fun() -> ?assert(true) end,
        ?_assert(true)
    ]}.

pass() -> ?assert(true).
```

Here, we wrap a list of three tests with setup/cleanup functions. The setup function is called, then the tests are run,
then the cleanup function is called (even if any of the tests fail).

Note that this is a test generator/instantiator, rather than a simple test. This is denoted by the underscore in the
function name: `...test_()`. This is *required*, and EUnit won't tell you if you forget.

<div class="callout callout-info" markdown="span">
**Tip:** Start with `?assert(false)` as your test, to make sure it's actually being run.
</div>

This example also shows a few of the different ways that you can specify a test:

- As a function reference -- `fun pass/0`.
- As an anonymous function -- `fun() -> ?assert(true) end`.
- Using one of the test-generator macros (the ones starting with an underscore) -- `?_assert(true)`.

If you prefer (maybe you've used JUnit in the past), you can think of `suite_setup` and `suite_cleanup` as `before_all`
and `after_all`, respectively.

## `foreach`

If you want to run some setup/cleanup before _each_ test in a list, you can use `foreach`:

```erlang
simple_foreach_test_() ->
    % 'foreach' sets up a fixture for each of the specified tests.
    {foreach, fun setup/0, fun cleanup/1, [
        fun pass/0,
        fun() -> ?assert(true) end,
        ?_assert(true)
    ]}.
```

If you're coming from JUnit or similar, these are `before_each` and `after_each`.

## Nesting `setup` and `foreach`

If you want to run some setup before (and cleanup after) a list of tests, and _also_ some setup and cleanup for each
test, you can nest `setup` and `foreach`:

```erlang
nested_setup_foreach_test_() ->
    {setup, fun before_all/0, fun after_all/1,
        {foreach, fun before_each/0, fun after_each/1, [
            fun pass/0,
            fun() -> ?assert(true) end,
            ?_assert(true)
        ]}}.
```

## Passing the result from `setup`

The result from `setup` is passed to `cleanup` already. What if we want to pass it to each of our tests? It's a bit
awkward, but it looks like the following:

```erlang
suite_setup() ->
    {ok, Pid} = some_server:start_link(),
    Pid.

suite_cleanup(Pid) ->
    exit(Pid, kill),
    ok.

setup_result_test_() ->
    % the result of Setup can be passed to each test, provided you wrap them in an instantiator:
    {setup, fun suite_setup/0, fun suite_cleanup/1, fun(Pid) ->
        [
            fun pass/0,
            fun() -> ?assert(true) end,
            ?_assert(true)
        ]
    end}.

setup_result_test_nope() ->
    % It doesn't want a list of instantiators, just an instantiator.
    {setup, fun suite_setup/0, fun suite_cleanup/1, [
        fun(_X) -> fun() -> ?assert(true) end end,
        fun(_X) -> ?_assert(true) end
    ]}.
```

TODO: Are we _sure_ we can't have a list of generators? That'd be much cleaner.

```erlang
simple_setup_single_test_() ->
    % 'setup' can take a single test; doesn't need to be a list.
    {setup, fun suite_setup/0, fun suite_cleanup/1, ?_assert(true)}.

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

foreach_result_test_() ->
    % 'foreach' MUST have a list, but it can be a list of instantiators.
    {foreach, fun setup/0, fun cleanup/1, [
        fun(_X) -> fun() -> pass() end end,
        fun(_X) -> fun() -> ?assert(true) end end,
        fun(_X) -> ?_assert(true) end
    ]}.

foreach_result_single_instantiator_test_() ->
    % ...or a list with a single instantiator, which returns a list.
    {foreach, fun setup/0, fun cleanup/1, [fun(_X) ->
        [
            fun pass/0,
            fun() -> ?assert(true) end,
            ?_assert(true)]
    end]}.

nested_setup_result_foreach_test_() ->
    % Because 'setup' accepts an instantiator, you can pass the result to the tests in foreach (by currying them, essentially):
    {setup, fun suite_setup/0, fun suite_cleanup/1, fun(Suite) ->
        {foreach, fun setup/0, fun cleanup/1, [
            fun() -> ?assertEqual(suite_setup, Suite) end
        ]}
    end}.

nested_setup_result_foreach_with_test_() ->
    % We can avoid the currying by using 'with'...
    % It's actually _more_ verbose though.
    {setup, fun suite_setup/0, fun suite_cleanup/1, fun(Suite) ->
        {foreach, fun setup/0, fun cleanup/1, [
            {with, Suite, [
                fun(X) -> ?assertEqual(suite_setup, X) end
            ]}
        ]}
    end}.

nested_setup_result_foreach_with_test_nope() ->
    % foreach STILL needs the list, though.
    {setup, fun suite_setup/0, fun suite_cleanup/1, fun(Suite) ->
        {foreach, fun setup/0, fun cleanup/1,
            {with, Suite, [
                fun(X) -> ?assertEqual(suite_setup, X) end
            ]}}
    end}.

nested_setup_result_foreach_result_test_() ->
    % This works, even though it's messy.
    {setup, fun suite_setup/0, fun suite_cleanup/1, fun(Suite) ->
        {foreach, fun setup/0, fun cleanup/1, [
            fun(ForEach) ->
                fun() ->
                    ?assertEqual(suite_setup, Suite),
                    ?assertEqual(setup, ForEach)
                end
            end
        ]}
    end}.

setup1(Suite) ->
    {Suite, ?FUNCTION_NAME}.

nested_setup_result_foreach_tuple_result_test_() ->
    % Or you could pass the suite context to setup, and have it return a tuple/record/map:
    {setup, fun suite_setup/0, fun suite_cleanup/1, fun(Suite) ->
        {foreach, fun() -> setup1(Suite) end, fun cleanup/1, [
            fun(Tuple) ->
                ?_assertEqual({suite_setup, setup1}, Tuple)
            end,
            fun expect_tuple_/1
        ]}
    end}.

% underscore to go with the convention on generators.
expect_tuple_(Tuple) -> ?_assertEqual({suite_setup, setup1}, Tuple).
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
