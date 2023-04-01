---
title: "EUnit examples: setup"
short_title: "setup"
date: 2023-03-31T08:49:00.000Z
tags: erlang eunit
layout: series
series: erlang-eunit-examples
---

What if we want to run some setup before a set of tests (and cleanup afterwards)?

```erlang
suite_setup() ->
    % Setup for a group of tests goes here.
    % For example:
    {ok, Pid} = some_server:start_link(),
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
        ?_assert(true),
        {"description of test", ?_assert(true)}
    ]}.

pass() -> ?assert(true).
```

Here, we wrap a list of three tests with setup/cleanup functions. The setup function is called, then the tests are run,
then the cleanup function is called (even if any of the tests fail).

Note that this is a test generator/instantiator, rather than a simple test. This is denoted by the underscore in the
function name: `...test_()`. This is *required*, and EUnit won't always tell you if you forget.

<div class="callout callout-info" markdown="span">
**Tip:** Start with `?assert(false)` as your test, to make sure it's actually being run.
</div>

This example also shows a few of the different ways that you can specify a test:

- As a local function reference -- `fun pass/0`.
- As an anonymous function -- `fun() -> ?assert(true) end`.
- Using one of the test-generator macros (the ones starting with an underscore) -- `?_assert(true)`.
- Using a description. This must be a string or a binary.

If you prefer (maybe you've used JUnit in the past), you can think of `suite_setup` and `suite_cleanup` as `before_all`
and `after_all`, respectively.
