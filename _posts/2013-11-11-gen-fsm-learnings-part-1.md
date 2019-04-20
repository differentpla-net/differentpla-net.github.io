---
title: gen_fsm learnings, part 1
date: 2013-11-11T15:31:10Z
tags: erlang
layout: series
series: gen_fsm
---
## Background

All of the `gen_fsm` examples I've found on the Internet are way too
complicated. For example, the one in [learn you some
Erlang](http://learnyousomeerlang.com/finite-state-machines) jumps from a
simple, hand-written FSM to one that seems to handle a full trading floor
simulation. That's a steep learning curve in my book.

Here's a simpler example.

## The goal

A simple state machine with two states: `alpha` and `beta`. It starts in state `alpha`,
and you can send it between the states by sending it messages.

## Getting started

First, we'll need a source file. Call it `ab_fsm.erl`, and start with the
following:

    -module(ab_fsm).
    -behaviour(gen_fsm).

Now, if we compile that (`erlc ab_fsm.erl`), we'll get warnings telling us which bits we forgot
-- this is *proper* TDD:

    ab_fsm.erl:2: Warning: undefined callback function code_change/4 (behaviour 'gen_fsm')
    ab_fsm.erl:2: Warning: undefined callback function handle_event/3 (behaviour 'gen_fsm')
    ab_fsm.erl:2: Warning: undefined callback function handle_info/3 (behaviour 'gen_fsm')
    ab_fsm.erl:2: Warning: undefined callback function handle_sync_event/4 (behaviour 'gen_fsm')
    ab_fsm.erl:2: Warning: undefined callback function init/1 (behaviour 'gen_fsm')
    ab_fsm.erl:2: Warning: undefined callback function terminate/3 (behaviour 'gen_fsm')

So, let's export those...

    -export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

We'll also want a `start_link` function:

    -export([start_link/0]).

    start_link() ->
      Args = [], Options = [],
      gen_fsm:start_link(?MODULE, Args, Options).

...and start defining them. The `init` function is called first, to do any
initialisation (based on `Args`). It should return `{ok, InitialStateName,
InitialStateData}`.

    init(_Args) ->
      {ok, alpha, []}.

I'm not entirely sure (yet) what the next three are for, so I'll just stub them out:

    handle_event(_Event, StateName, StateData) ->
      {next_state, StateName, StateData}.

    handle_sync_event(_Event, _From, StateName, StateData) ->
      {reply, ok, StateName, StateData}.

    handle_info(_Info, StateName, StateData) ->
      {next_state, StateName, StateData}.

`terminate` is called to do cleanup; we don't have anything to do:

    terminate(_Reason, _StateName, _StateData) ->
      ok.

`code_change` is called once the new code is loaded to upgrade the state; we don't do anything:

    code_change(_OldVsn, StateName, StateData, _Extra) ->
      {ok, StateName, StateData}.

## Let's try it out

Now we can try it out a bit:

    $ erl
    1> c(ab_fsm).
    {ok,ab_fsm}
    2> {ok, Pid} = ab_fsm:start_link().
    {ok,<0.41.0>}

Well, it's running. You might have noticed that `gen_fsm` requires that you
define a function for each state; we didn't do that. This, of course, means that
it doesn't work:

    3> gen_fsm:sync_send_event(Pid, foo).

    =ERROR REPORT==== 9-Nov-2013::11:20:55 ===
    ** State machine <0.41.0> terminating
    ** Last message in was {'$gen_sync_event',{<0.33.0>,#Ref<0.0.0.102>},foo}
    ** When State == alpha
    **      Data  == []
    ** Reason for termination =
    ** {'function not exported',
           [{ab_fsm,alpha,[foo,{<0.33.0>,#Ref<0.0.0.102>},[]],[]},
            {gen_fsm,handle_msg,7,[{file,"gen_fsm.erl"},{line,505}]},
            {proc_lib,init_p_do_apply,3,[{file,"proc_lib.erl"},{line,239}]}]}
    ** exception exit: undef
         in function  ab_fsm:alpha/3
            called as ab_fsm:alpha(foo,{<0.33.0>,#Ref<0.0.0.102>},[])
         in call from gen_fsm:handle_msg/7 (gen_fsm.erl, line 505)
         in call from proc_lib:init_p_do_apply/3 (proc_lib.erl, line 239)

I guess that we'd better go and implement the missing functions, then:

    -export([alpha/3]).
    alpha(beta, _From, StateData) ->
      {reply, ok, beta, StateData}.

Because we used `gen_fsm:sync_send_event`, this particular variant of `alpha`
gets called. It's supposed to send a reply (because it's synchronous).

This is Erlang, so we can simply reload the code and try again:

    16> c(ab_fsm).
    17> {ok, Pid} = ab_fsm:start_link().
    18> gen_fsm:sync_send_event(Pid, beta).

Cool; that works. Let's implement a bunch more, including changing `alpha`, so
that if it doesn't recognise the event, it stays in the same state. This
replaces the above snippet:

    -export([alpha/3, beta/3]).

    alpha(beta, _From, StateData) ->
      {reply, ok, beta, StateData};
    alpha(_Event, _From, StateData) ->
      {reply, ok, alpha, StateData}.

    beta(alpha, _From, StateData) ->
      {reply, ok, alpha, StateData};
    beta(_Event, _From, StateData) ->
      {reply, ok, beta, StateData}.

Note that these use clause matching to either handle a specified event (an
atom), or to handle all unknown events.

## Unit tests

And, we'll write some unit tests, because I'm bored with trying everything
manually:

    -include_lib("eunit/include/eunit.hrl").

    starts_test() ->
        {ok, _Pid} = ab_fsm:start_link().

    alpha_to_beta_test() ->
        {ok, Pid} = ab_fsm:start_link(),
        ok = gen_fsm:sync_send_event(Pid, beta).

    alpha_to_alpha_test() ->
        {ok, Pid} = ab_fsm:start_link(),
        ok = gen_fsm:sync_send_event(Pid, alpha),
        ok = gen_fsm:sync_send_event(Pid, nonsense).

    beta_to_alpha_test() ->
        {ok, Pid} = ab_fsm:start_link(),
        ok = gen_fsm:sync_send_event(Pid, beta),
        ok = gen_fsm:sync_send_event(Pid, alpha).

    beta_to_beta_test() ->
        {ok, Pid} = ab_fsm:start_link(),
        ok = gen_fsm:sync_send_event(Pid, beta),
        ok = gen_fsm:sync_send_event(Pid, nonsense).

We can test this more easily from the shell:

    1> c(ab_fsm).
    ok
    2> eunit:test(ab_fsm).
      All 5 tests passed.
    ok
