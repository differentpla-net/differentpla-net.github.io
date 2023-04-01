---
title: "EUnit examples: setup"
short_title: "setup"
date: 2023-03-31T08:20:00.000Z
tags: erlang eunit
layout: series
series: erlang-eunit-examples
---

What if we want to run some setup before a set of tests (and cleanup afterwards)?

```erlang
simple_setup_test_() ->
    % 'setup' sets up a single fixture for running all of the specified tests.
    {setup, fun suite_setup/0, fun suite_cleanup/1, [
        fun something/0,
        fun another_thing/0
    ]}.

suite_setup() ->
    % Setup for a group of tests goes here.
    % For example:
    {ok, Pid} = some_server:start_link(),
    Pid.

suite_cleanup(Pid) ->
    % The result from suite_setup is passed as the argument to suite_cleanup.
    unlink(Pid),    % ...so we don't also get killed
    exit(Pid, kill),
    ok.

something() -> ?assert(true).
another_thing() -> ?assert(true).
```

Here, we wrap a list of three tests with setup/cleanup functions. The setup function is called, then the tests are run,
then the cleanup function is called (even if any of the tests fail).

If you prefer (maybe you've used JUnit in the past), you can think of `suite_setup` and `suite_cleanup` as `before_all`
and `after_all`, respectively.

Note also that, because we called `some_server:start_link()`, we're linked to the server process. When we kill it in
`cleanup`, this will occasionally kill the test process as well. To avoid this, we call `unlink` first. There are better
ways to deal with this; I'll discuss those later.
