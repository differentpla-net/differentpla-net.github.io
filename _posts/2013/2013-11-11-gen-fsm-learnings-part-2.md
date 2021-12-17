---
title: gen_fsm learnings, part 2
date: 2013-11-11T16:12:36Z
tags: erlang
layout: series
series: gen_fsm
---
## `handle_event` and `handle_sync_event`

OK. So, looping back round, just what _are_ `handle_event` and
`handle_sync_event` for?

The documentation states that `handle_event` is called from
`send_all_state_event`; it's intended to handle events for which every state
does the same thing. Similarly, `handle_sync_event` is called from
`sync_send_all_state_event`.

This, initially, seems odd: how is the caller supposed to know which states do
the same thing?

However, thinking about it some more: the caller's not supposed to call the
`gen_fsm` functions directly. **The caller's not supposed to know that we
implemented our state machine using `gen_fsm`.**

This means that we _should_ have provided `send_alpha` and `send_beta`
functions (or similar), and we should write our unit tests like this:

    alpha_to_beta_test() ->
      {ok, Pid} = ab_fsm:start_link(),
      ok = sync_send_beta(Pid).

    alpha_to_alpha_test() ->
      {ok, Pid} = ab_fsm:start_link(),
      ok = sync_send_alpha(Pid),
      ok = sync_send_alpha(Pid).

    beta_to_alpha_test() ->
      {ok, Pid} = ab_fsm:start_link(),
      ok = sync_send_beta(Pid),
      ok = sync_send_alpha(Pid).

    beta_to_beta_test() ->
      {ok, Pid} = ab_fsm:start_link(),
      ok = sync_send_beta(Pid),
      ok = sync_send_beta(Pid).

...and we should add the following functions:

    sync_send_alpha(Pid) ->
      gen_fsm:sync_send_event(Pid, alpha).
    sync_send_beta(Pid) ->
      gen_fsm:sync_send_event(Pid, beta).

## Synchronous events

You'll note that we've been using the `gen_fsm:sync_send_event` function. This
makes a synchronous call to `Module:StateName/3`, such as:

    alpha(_Event, _From, StateData) ->
      {reply, ok, alpha, StateData}.

Most of the time, you'll want to return `{reply, Result, NextStateName,
NewStateData}` -- check the [documentation][gen-fsm-ms3] for other possible
return values.

The `Result` is the second member of the tuple, and is returned to the caller,
synchronously. This is the `ok` we've been expecting in our unit tests. We
could have written instead:

    alpha(beta, _From, StateData) ->
      {reply, moved_to_beta, beta, StateData}.

...and our test would have looked like this:

    moved_to_beta = send_beta(Pid).

## Asynchronous events

If, instead, we decide to use asynchronous events, we should use
`gen_fsm:send_event`, as follows:

    send_alpha(Pid) ->
      ok = gen_fsm:send_event(Pid, alpha).

That is: `send_event` sends the event asynchronously, and returns `ok`
immediately. Instead of calling `Module:StateName/3`, which -- you'll recall --
is provided with the caller's Pid, it calls `Module:StateName/2`:

    alpha(beta, StateData) ->
      {next_state, beta, StateData}.

I guess that you'd decide which events are synchronous, and which are
asynchronous ahead of time, based on whether you need a reply, or whether you
need to hold something up.
