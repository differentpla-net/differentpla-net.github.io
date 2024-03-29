---
title: "EUnit examples: Using 'foreach'"
short_title: "Using 'foreach'"
date: 2023-03-31T08:30:00.000Z
tags: erlang eunit
layout: series
series: erlang-eunit-examples
---

We saw that `setup` runs setup/cleanup before _all_ tests in a list. If you want to run some setup/cleanup before _each_
test in a list, you can use `foreach`:

```erlang
simple_foreach_test_() ->
    % 'foreach' sets up a fixture for each of the specified tests.
    {foreach, fun setup/0, fun cleanup/1, [
        fun something/0,
        fun another_thing/0
    ]}.

setup() ->
    {ok, Pid} = some_server:start_link(),
    Pid.

cleanup(Pid) ->
    % The result from setup is passed as the argument to cleanup.
    unlink(Pid),    % ...so we don't also get killed
    exit(Pid, kill),
    ok.

something() -> ?assert(true).
another_thing() -> ?assert(true).
```

If you're coming from JUnit or similar, these are `before_each` and `after_each`.
