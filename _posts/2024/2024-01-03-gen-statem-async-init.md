---
title: "gen_statem: Asynchronous Initialization"
date: 2024-01-03T14:11:00Z
tags: erlang
---

When you use `gen_statem:start/3,4` or `gen_statem:start_link/3,4`, the call blocks until the server process has finished running `Module:init/1`.

If `Module:init/1` takes a long time to run, this can cause several problems:

- If your process is being started from a supervisor, the supervisor will be unable to handle any messages until
  `init/1` completes. This will result in calls such as `supervisor:which_children/1` blocking, or delays in restarting
  other supervised processes.
- If you want to start a number of processes, they'll be initialised sequentially, rather than in parallel. This can
  result in slow startup for your application.

Here are a few ways to mitigate the problem. They're not all appropriate for all situations.

## `enter_loop`

_Note: don't actually do this; there are better solutions._

In a [previous blog post]({% post_url 2018/2018-06-12-gen-server-enter-loop %}), I showed how to solve this for
`gen_server` by using `gen_server:enter_loop/3,4,5`. You can the same thing for `gen_statem`, as follows:

```erlang
start_link(Host, Port) when is_list(Host), is_integer(Port) ->
    % Pass Args as a list containing a list, so that init/1 is called, for compatibility with the behaviour.
    proc_lib:start_link(?MODULE, init, [[Host, Port]]).

init([Host, Port]) ->
    % For example:
    Opts = [{active, true}, {mode, binary}],
    {ok, Sock} = gen_tcp:connect(Host, Port, Opts),

    % We've done the synchronous bit; call init_ack to unblock start_link.
    proc_lib:init_ack({ok, self()}),

    % Continue asynchronously.
    StateData = #state{
        % ...
    },

    gen_statem:enter_loop(?MODULE, [], connected, StateData).
```

As a guideline, you want to divide `Module:init/1` into the following:

1. A synchronous, blocking piece. Any failures here will cause `start_link` to fail. Use this for anything that would be
   better handled by simply failing to start.
   See ["It's About the Guarantees"](https://ferd.ca/it-s-about-the-guarantees.html), by Fred Hebert, for more on this.
2. An asynchronous, non-blocking piece. At this point, you're claiming that the server is in a known state and can
   handle requests. Any failures here should have their own retry logic or you should just die and have the supervisor
   deal with it. The strategy you choose comes down to what guarantees you're claiming to offer.

## Looping

If you want to implement a loop in a `gen_statem`, you'll probably do this by returning something like `{next_event, internal, loop}` from the event handler. You can trigger this behaviour using `gen_statem:enter_loop/5,6`:

```erlang
start_link(Host, Port) when is_list(Host), is_integer(Port) ->
    proc_lib:start_link(?MODULE, init, [[Host, Port]]).

init([Host, Port]) ->
    % ... sync init ...
    proc_lib:init_ack({ok, self()}),
    % ... async init ...
    gen_statem:enter_loop(?MODULE, [], connected, StateData, [{next_event, internal, loop}]).

handle_event(internal, loop, _, StateData) ->
    % ... something interesting ...
    {keep_state_and_data, [{next_event, internal, loop}]}.
```

<div class="callout callout-warning" markdown="span">
Caution: if you do this, you'll find that your `gen_statem` won't respond to `call` events or system events. In
particular, you won't be able to ask it to stop. Consider inserting a short timeout action in between each iteration of
the loop. Or, if you're calling another process, use something like `send_request`...`check_response` to make it
asynchronous.
</div>

## `state_enter`

`gen_statem` can use **state enter calls**. You can use these as a way to defer initialization work, but with a **major caveat**.

```erlang
start_link(Host, Port) when is_list(Host), is_integer(Port) ->
    gen_statem:start_link(?MODULE, [Host, Port], []).

callback_mode() -> [state_enter, handle_event_function].

init([Host, Port]) ->
    % ...
    {ok, initializing, StateData}.

handle_event(enter, _, initializing, StateData) ->
    % do asynchronous initialization here.
    {next_state, connected, StateData};
handle_event(enter, _, _, _) ->
    % all other state enter events.
    keep_state_and_data;
handle_event(EventType, EventContent, State, StateData) ->
    % ...
```

This works pretty well, but the **major caveat** is that you're only allowed to return a restricted list of actions from
`handle_event(enter, ...)`. In particular, you can't return `{next_event, internal, Event}`, which means that you can't
easily start a looping action.

## Combining `init` and `next_event`

My currently preferred solution is the following. It can easily be combined with the further use of `next_event` to
trigger a loop.

```erlang
start_link(Host, Port) when is_list(Host), is_integer(Port) ->
    gen_statem:start_link(?MODULE, [Host, Port], []).

callback_mode() -> [handle_event_function].

init([Host, Port]) ->
    % ...
    {ok, initializing, StateData, {next_event, internal, initalize}}.

handle_event(internal, initialize, initializing, StateData) ->
    % do asynchronous initialization here.
    {next_state, connected, StateData, [{next_event, internal, loop}]};
handle_event(internal, loop, _, StateData) ->
    % ... something interesting ...
    {keep_state_and_data, [{next_event, internal, loop}]}.
```
