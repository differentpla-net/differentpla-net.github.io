---
title: "Using eunit to test private functions"
date: 2022-10-25T17:33:00Z
tags: erlang eunit
---

You've got an Erlang module with a private (not-exported) function, and you want to add some unit tests for that
function? How should you do that?

## Don't test private functions

There's an argument to be made for not testing private functions. They don't form part of your public interface, and you
should only test your module through the public interface.

This is a dogmatic argument, though.

A better argument against unit-testing private functions is that they are implementation details, and are subject to
change. By adding unit tests for your private functions, you've added extra coupling between the unit tests and your
code, and you've made it harder to evolve your code.

On the other hand, you might find yourself having to carefully arrange calls to the public interface in order to test
the relevant paths through the private function. This might make your tests more complicated than they need to be. That
leads to test brittleness, and discourages people from fearless refactoring.

Pragmatically, then, you might _still want_ to test the private function. How could you do that?

## Create a new module for the function

If you've decided that you're only testing public interfaces, and this function needs testing, then maybe it's more
important than you think. Maybe it should be in a separate module, and exported from that?

One potential problem with this solution is that you lose cohesion. You end up with either a `foo_utils` module, with a
grab-bag of stuff in it, or a bunch of small modules that don't seem to make any sense.

If you can find a few other related functions (maybe they're all to do with input validation, or date/time handling,
etc.), then this might be a good idea.

## Put the tests in the module

If your tests are in the relevant module, you don't need to export the function at all. This might be how you've been
writing your eunit tests anyway. If it is, then this is no big deal for you.

If you're going to do this, you'll do something like this:

```erlang
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

foo_test() -> ?assertEqual(42, meaning_of([life, universe, everything])).
-endif.
```

On the other hand, if you've already got some unit tests in the `test` directory of your project, it might make it
harder to find some of the tests if some of them are in the `src` directory.

## Export everything from the module

Some languages don't distinguish between public and private functions, except (possibly) by naming conventions, so
they're already doing this.

You probably wouldn't do this outside your test suite, because then callers (who've not been reading your documentation)
start using these private functions, which leads to more coupling, and you end up either supporting your private
functions forever, or you have a lot of work decoupling everything later.

What you _can_ do (in Erlang), is this:

```erlang
-module(foo).
% ...
-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-endif.

% ...
```

Or you can find a way to pass those compiler options on the command-line. If you're using `erlang.mk`, you're looking
for `TEST_ERLC_OPTS`:

```
TEST_ERLC_OPTS = +export_all +nowarn_export_all
```

I don't like this approach, because I feel that it lacks deliberate intent.

Plus there's the annoying wart to turn off the warning, literally next to the thing you've just turned on; see, for
example, [this mailing list post](http://erlang.org/pipermail/erlang-questions/2017-October/094056.html) explaining the
problem.

## Export just the functions that need testing

```erlang
-module(foo).
-export([start_link/1, ...]).

-ifdef(TEST).
-export([internal_function/2]).
-endif.

% ...
```

If I can't figure out a way to pull the functions into their own module, then this is my preferred option. It's
deliberate, and it makes it clear that you intended to export just the relevant functions.

{% capture warning %}
With `erlang.mk`, this technique runs into a **problem**.<br/>
`erlang.mk` doesn't keep the `.beam` files separate for `dev` vs. `test` targets, and doesn't notice that `-DTEST`
changed.<br/>
So it doesn't recompile the file, and you get inconsistent results depending on whether you last ran `make eunit` or
`make app`.<br/>
`rebar3` (and `mix`) keep the configurations separate (in `_build/dev`, e.g.), so you don't run into this problem.
{% endcapture %}
{% include alerts/warning.html content=warning %}

## Conclusions

Are there any? As with many things, it depends. Do what works for you.
