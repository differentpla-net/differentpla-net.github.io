---
title: "Using setup and foreach in eunit tests"
date: 2023-01-03T13:18:00.000Z
tags: erlang eunit
---

For when you want to run both per-suite and per-test setup and cleanup in eunit tests.

```erlang
the_test_() ->
    {setup, fun suite_setup/0, fun suite_cleanup/1,
        {foreach, fun setup/0, fun cleanup/1, [
            % tests go here
        ]}};
```
