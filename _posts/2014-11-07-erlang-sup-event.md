---
title: Supervised event handlers in Erlang
date: 2014-11-07 10:11:33Z
---

I find Erlang's `gen_event` behaviour to be fairly tricky to understand,
despite the copious documentation on the subject:

 - [gen_event Behaviour](http://www.erlang.org/doc/design_principles/events.html) (Erlang documentation)
 - [gen_event man page](http://www.erlang.org/doc/man/gen_event.html) (Erlang documentation)
 - [Event Handlers](http://learnyousomeerlang.com/event-handlers) (Learn You Some Erlang)

Where the documentation is weak, I think, is in explaining how to wire
`gen_event` into your application's supervision tree.

This is my attempt to explain it, although I'm going to make a detour into
implementing server-sent events in Cowboy.

To discover how the supervision works, I spent some hours poking around in the
source code for [lager](https://github.com/basho/lager), a popular logging
library for Erlang.

## The Event Manager

There are three parts to `gen_event`: the "event manager" (what other
frameworks would probably call the "event bus"), the event handlers, and the
event publishers.

We'll look at the event manager first.

This is created by `gen_event:start_link/0` or `gen_event:start_link/1`. You'll
usually give your event manager a name -- otherwise you need to remember the
pid.

Most examples show this as:

    {ok, Pid} = gen_event:start_link({local, my_event}).

So, how do we put this in our supervision tree?

    % somewhere in my_sup:init/1...
    Children = [
        {my_event, {gen_event, start_link, [{local, my_event}]},
            permanent, 5000, worker, [dynamic]}
        % , ...
    ].

## Adding Event Handlers

Great, now we've got an event manager, how do we add a handler?

The documentation has:

    ok = gen_event:add_handler(my_event, my_event_handler, []).

The question that it doesn't answer is "when do I do this?". To answer this
question, we need to realise that your application's supervision tree is built
*in order*. That is: when `supervisor:start_link` returns successfully, the
supervision tree is [completely initialised](http://ferd.ca/it-s-about-the-guarantees.html).

So:

    -module(my_sup).
    -behaviour(supervisor).
    -export([start_link/0, init/1]).

    start_link() ->
        {ok, Pid} = supervisor:start_link(?MODULE, []),
        ok = gen_event:add_handler(my_event, my_event_handler, []).
        {ok, Pid}.

    init([]) ->
        Children = [
            {my_event, {gen_event, start_link, [{local, my_event}]},
                permanent, 5000, worker, [dynamic]}
        ],
        {ok, { {one_for_one, 10, 60}, Children } }.

## Implementing Event Handlers

In the snippet above, we added a handler module `my_event_handler`. We should
probably implement that. To do that, we need:

    -module(my_event_handler).
    -export([start_link/0]).
    -behaviour(gen_event).
    
    % The rest is left as an exercise for the reader,
    % as one of my Discrete Maths lecturers was fond of saying :-)
    % ...

For details of the functions required, etc., consult the [gen_event
documentation](http://www.erlang.org/doc/man/gen_event.html#Module:init-1).

## Raising events

To raise an event -- to publish it to all currently-registered handlers, you
can simply use `gen_event:notify/2`:

    % 'Event' is whatever you want it to be. For example:
    Event = {job_started, JobId, JobParams, os:timestamp()},
    gen_event:notify(my_event, Event).

This will, for each `my_event` handler, call `Handler:handle_event(Event, State)`.

## notify and sync_notify

But `gen_event` has more than just `notify/2`, and the handler must implement
more than just `handle_event/2`.

 - `gen_event:notify/2` -- raise an event asynchronously. It returns
   immediately.
 - `gen_event:sync_notify/2` -- raise an event, and wait for all of the
   handlers to receive it.

## handle_info

`Handler:handle_info/2` is called whenever a normal process message is received
by the event manager process. You might be wondering why you'd send a normal
message to the event manager, given that it already has `notify/2`.

This might be useful if, for example, your event handler wants to call
`monitor` and receive the `'DOWN'` messages.

## call and handle_call

The difference between this and `notify` is that it allows you to easily send a
message to *a particular handler*:

    gen_event:call(my_event, my_event_handler, Request).

## Give me an example

Here's one: you're writing a simple notification server, using
[Cowboy](https://github.com/ninenines/cowboy). You're using server-sent events
to distribute the events to connected clients.

This means that you have a cowboy handler that looks something like this:

    -module(my_http_handler).
    % Note: cowboy 1.0, not master.
    -export([init/3, info/3, terminate/3]).

    init(_Type, Req, []) ->
        Headers = [{<<"content-type">>, <<"text/event-stream">>}],
        {ok, Req2} = cowboy_req:chunked_reply(200, Headers, Req),
        % @todo Subscribe to events...
        {loop, Req2, undefined, infinity}.

So, how _does_ the handler subscribe to the events? `gen_event` makes the
assumption that we want to distribute events to *modules*, not to *processes*.

We need an event handler that understands processes, and some way of
registering our cowboy handler process with that handler.

You could do this with some kind of process registry. The idea here is that
`my_event_handler` does something like this:

    % use gproc to send an {event, Event} message to every process
    % registered locally with the 'my_event_proc' property.
    handle_event(Event, State) ->
        gproc:send({p, l, my_event_proc}, {event, Event}),
        {ok, State}.

The cowboy handler (from above) would subscribe like this:

    init(_Type, Req, []) ->
        % ...
        gproc:reg({p, l, my_event_proc}),
        % ...

Then it could send the event to the connected client like this:

    info({event, Event}, State) ->
        % Convert the event to text (somehow):
        Data = to_text(Event),
        ok = cowboy_req:chunk(["data: ", Data, "\n", "\n"], Req),
        {loop, Req, State}.

As another example, I found the [wrinqle](https://github.com/brickcap/wrinqle)
library which uses `pg2` instead of `gproc`.

That's great, but what if we don't want to take a dependency on a process
registry?

That's where you might use `gen_event:call`:

    % in my_http_handler
    init(_Type, Req, []) ->
        % ...
        % call our handler specifically.
        ok = gen_event:call(my_event, my_event_handler, {register, self()}),
        % ...

    % in my_event_handler
    handle_call({register, Pid}, #state{ subs = Subs } = State) ->
        monitor(process, Pid),
        State2 = State#state{ subs = [Pid|Subs] },
        {ok, ok, State2}.

...and you'd need to remember to handle `'DOWN'` messages in
`my_event_handler:handle_info`. See? I wasn't making that part up either.

## Supervised handlers

Where was I? Oh yeah, we're supposed to be talking about supervised handlers.

LYSE talks about `add_sup_handler`
[here](http://learnyousomeerlang.com/event-handlers#highlighter_642111), and
mentions some of the problems you might have with them.

In short, you need an event handler guard to restart any crashed handlers.
There's an example of that [on Erlang
Central](https://erlangcentral.org/wiki/index.php?title=Gen_event_behavior_demystified#Fault_Tolerance),
but I felt that it leaves out some of the details.

However, the lager source code provides a particularly good example of
supervised handlers, so I've taken that apart and I'll attempt to explain it
here.

The secret sauce is that you need a process to "supervise" the handler. This is
hinted at in the documentation for `gen_event:add_sup_handler`:

> Adds a new event handler in the same way as add_handler/3 but will also
> supervise the connection between the event handler and the calling process.

What this actually _means_ is that, for each handler, you need a process to
handle `gen_event_EXIT` messages and (optionally) restart the handler. That
process is *not* a supervisor, but it *does* need one.

### Top-level supervisor

Let's start at the top. Here's a top-level supervisor:

    -module(my_event_sup).
    -behaviour(supervisor).
    -export([start_link/0, init/1]).

    start_link() ->
        % The name is optional.
        supervisor:start_link(?MODULE, []).

    init([]) ->
        Children = [
            % event manager
            {my_event, {gen_event, start_link, [{local, my_event}]},
                permanent, 5000, worker, [dynamic]},
            % event handler guard supervisor
            {my_event_guard_sup, {my_event_guard_sup, start_link, []},
                permanent, 5000, supervisor, [my_event_guard_sup]}
        ],
        {ok, { {one_for_one, 10, 60}, Children } }.

This creates a supervisor with two children: the event manager and another
supervisor. This child supervisor will be the supervisor for the event handler
guards.

### Guard Supervisor

Here's the guard supervisor:

    -module(my_event_guard_sup).
    -behaviour(supervisor).
    -export([start_link/0, init/1]).

    start_link() ->
        Name = {local, ?MODULE},
        supervisor:start_link(Name, ?MODULE, []).

    init([]) ->
        % This is a 'simple_one_for_one' supervisor, so this must be a single
        % child spec.
        Children = [
            {my_event_guard, {my_event_guard, start_link, []},
                temporary, 5000, worker, [my_event_guard]}
        ],
        {ok, { {simple_one_for_one, 10, 60}, Children } }.

Some points of interest:

 - The supervisor is `simple_one_for_one`. This means that all of the children
   must be the same and that no children are started until `start_child` is
   called. See the [supervisor
   documentation](http://www.erlang.org/doc/man/supervisor.html#Module:init-1).
 - We use 'temporary' for the restart strategy; this means that the supervisor
   will never restart the child. Lager uses this; you might want 'transient' or
   'permanent' instead.

### Guard Process

The guard process starts with the usual boilerplate:

    -module(my_event_guard).
    -behaviour(gen_server).
    -export([start_link/3]).
    -export([init/1, handle_call/3, handle_cast/2, handle_info/2,
             terminate/2, code_change/3]).

Then it gets a bit more complex. We define `start_link` as follows:

    start_link(Event, Module, Config) ->
        gen_server:start_link(?MODULE, [Event, Module, Config], []).

Wait. Where did those arguments come from? We didn't mention them in the child
spec.

The deal here is that, for `simple_one_for_one` supervisors, the call to
`supervisor:start_child` *appends* its arguments to the ones in the child spec.

So, if we call `supervisor:start_child(my_event_guard_sup, [my_event,
my_event_handler, []])`, then those arguments will be appended to the empty
list in the child spec, and will result in a call to `my_event_guard:start_link/3`.

And that calls `gen_server:start_link`, passing those arguments, which results
in a call to `my_event_guard:init/1`, which installs the given event handler,
and remembers the details for later:

    -record(state, {event, module, config}).

    init([Event, Module, Config]) ->
        install_handler(Event, Module, Config),
        {ok, #state{event=Event, module=Module, config=Config}}.

    install_handler(Event, Module, Config) ->
        ok = gen_event:add_sup_handler(Event, Module, Config).

Ah, finally, the call to `add_sup_handler`. Note that lager does something a
bit more
[complicated](https://github.com/basho/lager/blob/master/src/lager_handler_watcher.erl#L91)
here.

Then we can handle the `gen_event_EXIT` messages to restart (or not) the handler:

    handle_info({gen_event_EXIT, Module, normal}, #state{module=Module} = State) ->
        {stop, normal, State};
    handle_info({gen_event_EXIT, Module, shutdown}, #state{module=Module} = State) ->
        {stop, normal, State};
    handle_info({gen_event_EXIT, Module, Reason},
            #state{event=Event, module=Module, config=Config} = State) ->
        install_handler(Event, Module, Config),
        {noreply, State}.

### Adding Supervised Event Handlers

So, again, when do I add my event handlers?

Lager does it in `lager_app`. If we were to use that idea, it'd look something
like this:

    start(_Type, _Args) ->
        Handlers = [
            {my_event_handler, []},
            {other_event_handler, [foo, bar, baz]}
        ],

        {ok, Pid} = my_event_sup:start_link(),
        lists:foreach(
            fun({Module, Config}) ->
                supervisor:start_child(my_event_guard_sup, [my_event, Module, Config])
            end, Handlers),
        {ok, Pid}.

Alternatively, you could use similar code in the top-level supervisor, as we
did earlier.

## Conclusion

And that's pretty much it.

The key take-away here is that, when the documentation says:

> Adds a new event handler in the same way as add_handler/3 but will also
> supervise the connection between the event handler and the calling process.

It actually means:

- You need a guard process to call `add_sup_handler` for the handler.
- The guard process needs to handle `gen_event_EXIT` messages.
- You probably want a guard process for each handler, though you don't strictly
  _need_ this.
- The guard processes need a supervisor.
- That guard supervisor needs a parent supervisor.
- The parent supervisor should also supervise the event manager.
