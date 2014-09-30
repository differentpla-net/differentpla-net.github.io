---
title: gen_fsm learnings, part 3
date: 2013-11-12T13:51:52Z
---
Some odds and ends.

## `handle_info` information

The last of the "I don't know what this is for" functions is `handle_info`. This
is documented as being called for messages that aren't events or system
messages.

Your implementation of this should take a look at the event (in `Info`), and
then it can return `{next_state, NextStateName, NewStateData}` to move to the
next state.

But this isn't particularly state-machine-like, since you're not looking at the
current state when working out what to do with the event.

One possible answer (from [here](http://stackoverflow.com/a/3962655/8446)) is
to simply write it as follows:

    handle_info(Info, StateName, StateData) ->
        ?MODULE:StateName(Info, StateData).

But, if you have a choice, you should try to avoid sending messages directly to
your FSM process.

## Entering a state

I can't find any way to be notified when the FSM enters a particular state.
That is, there's nothing like `Module:State_enter(StateData)` or
`Module:handle_enter(StateName, StateData)`. So, you have to handle this case
when transitioning *from* the previous state.

Something like the following might suit:

    enter_gamma(StateData) ->
        %% Code goes here
        {next_state, gamma, StateData}.

    alpha(gamma, StateData) ->
        enter_gamma(StateData).

    beta(gamma, StateData) ->
        enter_gamma(StateData).

## Timeouts

You can have timeouts in your FSM, by returning `{next_state, NextStateName,
NewStateData, Timeout}`. If, while in the given state, no events are received,
you'll receive a call to `Module:State(timeout, StateData)`.

You can choose to do something and then remain in this state, or you could
transition to another state.

If you return `{next_state, SameState, NewStateData}`, the timeout is
discarded. If you return `{next_state, SameState, NewStateData, Timeout}`, the
timeout is restarted. If you want the timeout to continue, you'll need to keep
track of the time remaining yourself. See, for example, [my
answer](http://stackoverflow.com/a/19928265/8446) on Stack Overflow about this.

Note, however, that I'm not sure how the timeouts are calculated if you turn
`handle_info` into a direct call to the `Module:State/2` function, as above.

## How do I detect current state?

You can detect the current state of your FSM by using `sys:get_state/1`, as follows:

    {StateName, StateData} = sys:get_state(Pid).

Note that this won't work reliably for asynchronous events (race condition).
