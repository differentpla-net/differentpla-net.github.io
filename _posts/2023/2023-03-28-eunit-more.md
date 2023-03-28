---
title: "More eunit stuff"
date: 2023-03-28T08:47:00.000Z
tags: erlang eunit
---

TODO: Some stuff about how eunit finds tests goes here.

```erlang
simple_setup_test_() ->
    % 'setup' sets up a single fixture for running all of the specified tests.
    {setup, fun suite_setup/0, fun suite_cleanup/1, [
        fun() -> ?assert(true) end,
        fun() -> ?assert(true) end
    ]}.

simple_foreach_test_() ->
    % 'foreach' sets up a fixture for each of the specified tests.
    {foreach, fun setup/0, fun cleanup/1, [
        fun() -> ?assert(true) end,
        fun() -> ?assert(true) end
    ]}.

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

```erlang
suite_setup() ->
    ?LOG_INFO("~p", [?FUNCTION_NAME]),
    ?FUNCTION_NAME.

suite_cleanup(X) ->
    ?LOG_INFO("~p: ~p", [?FUNCTION_NAME, X]),
    ok.

setup() ->
    ?LOG_INFO("~p", [?FUNCTION_NAME]),
    ?FUNCTION_NAME.

cleanup(X) ->
    ?LOG_INFO("~p: ~p", [?FUNCTION_NAME, X]),
    ok.

pass() -> ?assert(true).

simple_setup_test_() ->
    % 'setup' sets up a single fixture for running all of the specified tests.
    {setup, fun suite_setup/0, fun suite_cleanup/1, [
        fun pass/0,
        fun() -> ?assert(true) end,
        ?_assert(true)
    ]}.

simple_setup_single_test_() ->
    % 'setup' can take a single test; doesn't need to be a list.
    {setup, fun suite_setup/0, fun suite_cleanup/1, ?_assert(true)}.

simple_foreach_test_() ->
    % 'foreach' sets up a fixture for each of the specified tests.
    {foreach, fun setup/0, fun cleanup/1, [
        fun pass/0,
        fun() -> ?assert(true) end,
        ?_assert(true)
    ]}.

simple_foreach_single_test_nope() ->
    % 'foreach' MUST have a list.
    {foreach, fun setup/0, fun cleanup/1, fun pass/0}.

nested_setup_foreach_test_() ->
    % You can nest 'setup' and 'foreach'.
    {setup, fun suite_setup/0, fun suite_cleanup/1,
        {foreach, fun setup/0, fun cleanup/1, [
            fun() -> ?assert(true) end,
            fun() -> ?assert(true) end
        ]}}.

setup_result_test_() ->
    % the result of Setup can be passed to each test, provided you wrap them in an instantiator:
    {setup, fun suite_setup/0, fun suite_cleanup/1, fun(_X) ->
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
