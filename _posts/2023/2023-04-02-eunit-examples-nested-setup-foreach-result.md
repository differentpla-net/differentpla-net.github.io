---
title: "EUnit examples: nested setup/foreach with results"
short_title: "nested setup/foreach with results"
date: 2023-04-02T11:28:00.000Z
tags: erlang eunit
layout: series
series: erlang-eunit-examples
---

If you've got nested `setup` and `foreach`, how do you pass the result from `suite_setup/0` -- in `{setup, fun
suite_setup/0...` -- to the tests?

If you do this:

```erlang
nested_setup_result_foreach_test_() ->
    % Because 'setup' accepts an instantiator, you can pass the result to the tests in foreach (by currying them, essentially):
    {setup, fun suite_setup/0, fun suite_cleanup/1,
        {foreach, fun setup/0, fun cleanup/1, [
            fun() -> ?assertEqual(suite_setup, Suite) end
        ]}
    end}.
```

```erlang
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
expect_tuple_(Tuple) ->
    ?_assertEqual({suite_setup, setup1}, Tuple).
```

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
