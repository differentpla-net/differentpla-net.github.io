---
title: "Erlang: via"
date: 2023-04-20T13:02:00.000Z
tags: erlang
---

Here's how you can use `via` in a name specification when starting a `gen_server` (or `gen_statem`, etc.) to register
your process with a process registry.

## Anonymous process

Ordinarily, when you start an Erlang `gen_server`, you'll use something like this:

```erlang
-module(anon_server).
-export([start_link/1]).
-behaviour(gen_server).
-export([init/1,
% etc.
]).

start_link() ->
    gen_server:start_link(?MODULE, [], [])
```

This tells Erlang to use `?MODULE` as the server implementation, doesn't pass any arguments to it, and doesn't pass any
`gen_server` options.

This is an anonymous process. We'll only be able to find it again if we store the process ID (pid). For example:

```erlang
{ok, Pid} = anon_server:start_link(),
% ...
```

Then we can call it like using the pid, for example:

```erlang
gen_server:call(Pid, hello).
```

Alternatively -- if it's supervised -- we can dig around in the supervisor tree.

## Named process

If we want a named process, we can do it like this:

```erlang
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
```

This puts the process in the built-in local process registry (it calls `erlang:register/2`). In most cases, you'll use
the module name as the server name, as here.

We can call it by name:

```erlang
gen_server:call(?SERVER, hello).
```

## Global process

Erlang also provides a cluster-wide process registry, named `global`. We can register with that as follows:

```erlang
start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).
```

I'll write something further about the `global` registry at some point. For example, it's not clear to me the best way
to ensure that your singleton process is always running somewhere in the cluster.

## Custom process registries

But sometimes you want to use a custom process registry. You _could_ do this by having your processes explicitly
register themselves. Here's an example using `gproc`:

```erlang
% gen_server, e.g.
init([]) ->
    Name = %...
    true = gproc:reg({n, l, Name}),
    {ok, #state{}}.
```

Here's another (possibly badly-remembered) example from a project I worked on a few years ago:

```erlang
% gen_server, e.g.
init([]) ->
    Name = {device_id, DeviceId},
    ei_registry:register_name(device_registry, Name),
    {ok, #state{}}.
```

I feel obliged to explain why we didn't use `via` in that case. First: I didn't know it existed at the time. Second: our
registry (similarly to `gproc`) allowed multiple names and properties per process, which doesn't neatly fit into how
`via` is defined.

## via

Or you can do it by using `via`:

```erlang
start_link() ->
    Name = %...
    gen_server:start_link({via, gproc, Name}, ?MODULE, [], []).
```

This takes advantage of the fact that `gproc` exports the functions needed to work with `via`.

Note that the name can be any Erlang term.
