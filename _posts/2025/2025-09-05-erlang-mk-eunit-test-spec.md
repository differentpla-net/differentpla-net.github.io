---
title: "erlang.mk: running multiple eunit tests"
date: 2025-09-05T13:35
tags: erlang-mk eunit
---

If you're using `erlang.mk`, you can run a single unit test suite with `make eunit t=whatever`. That's no good if you
want to run multiple test suites. Here's a nasty hack that makes it possible.

The motivation for this is that one test that fails when run as part of `make eunit`, but passes when run by itself.

We want to do some kind of binary chop to figure out where we've introduced a dependency, which means running multiple
test suites. You can't do this with `t=`. You _can_ do it by subverting `EUNIT_TEST_SPEC`.

The first thing to do is figure out which tests are being run by default. Add a target to your top-level Makefile, after including `erlang.mk`:

```makefile
list-tests:
	@echo $(call comma_list,$(EUNIT_MODS))
```

When you run it, you'll get a comma-separated list of the test modules.

Edit that list as appropriate -- e.g. you want to run the tests from a, b, c, and d -- then run the following:

```sh
make eunit "EUNIT_TEST_SPEC=[a,b,c,d]"
```

Note the quotes and the square brackets.
