---
title: gen_server:enter_loop
date: 2018-06-12 14:52
tags: erlang
---

When you use `gen_server:start/3,4` or `gen_server:start_link/3,4`, the call
blocks until the other process has finished running `init/1`.

In particular, this can be a problem when your process is being started from a
supervisor. If it takes a long time to start (or restart) the child process,
the supervisor won't handle any messages until it completes. This means that
even seemingly simple calls such as `supervisor:which_children/1` will block
until some unknown child process finishes (re-)starting.

## Don't do this

Occasionally, you'll see people suggest that you send yourself a message from
`init/1`, like this:

```erlang
start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    self() ! continue_init,
    {ok, #state{}}.

handle_info(continue_init, State) ->
    {noreply, continue_init_slowly()}.
```

Most of the time, you'll get away with this, because there's no way for anyone
else to send your process a message, and `continue_init` will be the first
message your process sees.

But: if your process registers a name, either through
`gen_server:start_link(Name, Module, Args, Opts)` or explicitly by calling
(e.g.) `register`, there opens a window where someone _could_ send you a
message which jumps the queue, arriving before `continue_init`, and before
you've finished initialising.

## `gen_server:enter_loop/3,4,5`

The *correct* way to do this is to use `gen_server:enter_loop`, combined with
either `proc_lib:spawn_link` or `proc_lib:start_link`.

### `proc_lib:spawn_link`

`proc_lib:spawn_link` returns immediately, and leaves the specified function to
run in the new process. In order to turn your new process into a `gen_server`,
you need to call `gen_server:enter_loop` at the end of `init`.

For example:

```erlang
start_link() ->
    Pid = proc_lib:spawn_link(?MODULE, init, [[]]),
    {ok, Pid}.

init([]) ->
    io:format("async init~n"),
    timer:sleep(1000),
    io:format("async init done~n"),
    gen_server:enter_loop(?MODULE, [], #state{}).
```

### `proc_lib:start_link`

If you want to do some synchronous initialisation, you can use `proc_lib:start_link`:

```erlang
start_link() ->
    % Returns the value passed to proc_lib:init_ack, below.
    proc_lib:start_link(?MODULE, init, [[]]).

init([]) ->
    io:format("sync init~n"),
    timer:sleep(1000),
    io:format("sync init done~n"),

    proc_lib:init_ack({ok, self()}),

    io:format("async init~n"),
    timer:sleep(1000),
    io:format("async init done~n"),

    gen_server:enter_loop(?MODULE, [], #state{}).
```
