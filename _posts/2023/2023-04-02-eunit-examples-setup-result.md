---
title: "EUnit examples: Passing the result from setup"
short_title: "Passing the result from setup"
date: 2023-04-02T08:20:00.000Z
tags: erlang eunit
layout: series
series: erlang-eunit-examples
---

The result from `setup` is passed to `cleanup` already. What if we want to pass it to each of our tests? It's a bit
awkward, but it looks like the following:

```erlang
suite_setup() ->
    {ok, Pid} = some_server:start_link(),
    Pid.

suite_cleanup(Pid) ->
    % Sending the 'kill' signal to the linked process also kills us, so trap exits.
    process_flag(trap_exit, true),
    exit(Pid, kill),
    ok.

setup_result_test_() ->
    % the result of Setup can be passed to each test, provided you wrap them in an instantiator:
    {setup, fun suite_setup/0, fun suite_cleanup/1, fun(_Pid) ->
        [
            fun something/0,
            fun another_thing/0
        ]
    end}.
```

You'll note that we're not actually passing `Pid` to the tests, just to the instantiator. To pass it to the tests, we
need to do something like this:

```erlang
setup_result_test_() ->
    % the result of Setup can be passed to each test, provided you wrap them in an instantiator:
    {setup, fun suite_setup/0, fun suite_cleanup/1, fun(Pid) ->
        [
            fun() -> something(Pid) end,
            fun() -> another_thing(Pid) end
        ]
    end}.

something(_Pid) -> ?assert(true).
another_thing(_Pid) -> ?assert(true).
```

Because of the anonymous functions -- `fun() -> something(Pid) end` -- the failure messages are, as mentioned in a
previous post, hard to read:

```
  1) setup_result_pid_test:-setup_result_test_/0-fun-1-/0
     Failure/Error: ?assert(false)
       expected: true
            got: false
```

You can fix that with the following:

```erlang
setup_result_test_() ->
    % the result of Setup can be passed to each test, provided you wrap them in an instantiator:
    {setup, fun suite_setup/0, fun suite_cleanup/1, fun(Pid) ->
        [
            something_(Pid),
            another_thing_(Pid)
        ]
    end}.

something_(_Pid) -> fun() -> ?assert(true) end.
another_thing_(_Pid) -> fun() -> ?assert(true) end.
```

The failure message is still not _great_, but it is _better_:

```
  2) setup_result_pid_test:-another_thing_/1-fun-1-/0
     Failure/Error: ?assert(false)
       expected: true
            got: false
```
