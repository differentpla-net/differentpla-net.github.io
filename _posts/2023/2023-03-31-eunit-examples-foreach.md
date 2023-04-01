---
title: "EUnit examples: foreach"
short_title: "foreach"
date: 2023-03-31T09:47:00.000Z
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
        fun some_test/0,
        fun another_test/0
    ]}.

setup() ->
    {ok, Pid} = some_server:start_link(),
    Pid.

cleanup(Pid) ->
    % The result from suite_setup is passed as the argument to suite_cleanup.
    unlink(Pid),    % ...so we don't also get killed
    exit(Pid, kill),
    ok.

some_test() -> ?assert(true).
another_test() -> ?assert(true).
```

If you're coming from JUnit or similar, these are `before_each` and `after_each`.
