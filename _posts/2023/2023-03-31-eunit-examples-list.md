---
title: "EUnit examples: a test list"
short_title: "A test list"
date: 2023-03-31T08:15:00.000Z
tags: erlang eunit
layout: series
series: erlang-eunit-examples
---

While you could specify each test with a single function, you can also use a test "instantiator" (or generator) and have
it return a list.

```erlang
list_of_test_() ->
    [
        fun something/0,
        fun() -> ?assert(true) end,
        ?_assert(true),
        {"description of test", ?_assert(true)},
        {<<"another test">>, fun another_thing/0}
    ].

something() -> ?assert(true).
another_thing() -> ?assert(true).
```

Note that this is a test generator/instantiator, rather than a simple test. This is denoted by the underscore in the
function name: `...test_()`. This is *required*, and EUnit won't always tell you if you forget.

This example also shows a few of the different ways that you can specify a test:

- As a local function reference -- `fun something/0`.
- As an anonymous function -- `fun() -> ?assert(true) end`.
- Using one of the test-generator macros (the ones starting with an underscore) -- `?_assert(true)`.
- Using a description. This must be a string or a binary.

<div class="callout callout-info" markdown="span">
**Tip:** Start with `?assert(false)` as your test, to make sure it's actually being run.
</div>

Note that using anonymous functions results in tests which are, well, anonymous.

This sometimes makes it hard to figure
out which test has failed. For example:

```
  2) list_of_test:-list_of_test_/0-fun-4-/0:8
     Failure/Error: ?assert(false)
       expected: true
            got: false
```

To get around this, consider using the variants that use named functions: `fun something/0`, or the descriptions.
