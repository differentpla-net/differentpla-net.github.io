---
title: "EUnit examples: Cleaning up server processes"
short_title: "Cleaning up server processes"
date: 2023-04-03T16:33:00.000Z
tags: erlang eunit
layout: series
series: erlang-eunit-examples
---

In several of the previous posts, for example [Using 'setup']({% post_url 2023/2023-03-31-eunit-examples-setup %}), I've
started a server in `suite_setup/0` and needed to kill it in `suite_cleanup/1`. I showed a simple way to do that, but
it's not the best. Here's a better way to do it.

To recap, I showed this:

```erlang
suite_setup() ->
    {ok, Pid} = some_server:start_link(),
    Pid.

suite_cleanup(Pid) ->
    unlink(Pid),    % ...so we don't also get killed
    exit(Pid, kill),
    ok.
```

Because of the call to `some_server:start_link/0`, the server process is linked to the test process. Specifically: it's
linked to the test _setup_ (and _cleanup_) process: the tests run in a separate process.

When cleaning up, we want to kill the server process, so we want to do something like this:

```erlang
suite_cleanup(Pid) ->
    exit(Pid, kill),
    ok.
```

But killing the server process also kills us (because we're linked to it).

There are a number of ways to deal with this. I showed the simplest: simply unlink from the server process first using
`unlink(Pid)`. Another way is to trap exits, as follows:

```erlang
suite_cleanup(Pid) ->
    process_flag(trap_exit, true),
    exit(Pid, kill),
    ok.
```

If you do that, then -- instead of killing the test process -- the exit signal is converted into a message which can be
ignored (and we do).

It's still not perfect, however. Consider the case where our server has a name. For example:

```erlang
-module(some_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_info/2, handle_cast/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
```

You'll soon discover that you've got a race condition: the server isn't stopped immediately, and you'll get
`{error,{already_started,Pid}}` errors in the next fixture setup.

To solve this, you'll need to wait until the server has actually stopped. The following will help:

```erlang
exit_and_wait(Pid, Reason) ->
    MRef = monitor(process, Pid),
    exit(Pid, Reason),
    receive
        {'DOWN', MRef, process, Pid, _Reason} ->
            ok
    end.
```

Use it as follows:

```erlang
suite_cleanup(Pid) ->
    process_flag(trap_exit, true),
    exit_and_wait(Pid, kill),
    ok.
```
