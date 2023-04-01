---
title: "EUnit examples: Nested setup and foreach"
short_title: "Nested setup and foreach"
date: 2023-04-02T08:14:00.000Z
tags: erlang eunit
layout: series
series: erlang-eunit-examples
---

If you want to run some setup before (and cleanup after) a list of tests, and _also_ some setup and cleanup for each
test, you can nest `setup` and `foreach`:

```erlang
nested_setup_foreach_test_() ->
    {setup, fun before_all/0, fun after_all/1,
        {foreach, fun before_each/0, fun after_each/1, [
            fun something/0,
            fun another_thing/0
        ]}}.
```

Here, I've used JUnit-style naming for the setup/cleanup functions.
