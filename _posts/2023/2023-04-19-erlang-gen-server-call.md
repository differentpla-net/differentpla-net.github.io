---
title: "Erlang: ways to specify the server in gen_server:call"
date: 2023-04-19T15:40:00.000Z
tags: erlang
---

You can `gen_server:call` in a bunch of different ways.

If you have a process ID:

```erlang
gen_server:call(Pid, Req).    % local by pid
```

If it's registered locally:

```erlang
gen_server:call(Name, Req).   % local by name -- uses whereis(Name)
gen_server:call({local, Name}, Req).    % local by name -- uses whereis(Name)
```

Note that `{local, Name}` is undocumented -- it's not listed under
[`server_ref()`](https://www.erlang.org/doc/man/gen_server.html#type-server_ref) and might not always be supported.

If it's registered in `global`:

```erlang
gen_server:call({global, Name}, Req).   % global by name -- uses global:whereis_name(Name)
```

If it's registered in some other process registry:

```erlang
gen_server:call({via, Registry, Name}, Req).  % via by name -- uses Registry:whereis_name(Name)
```

`global` uses the same interface as `via`:

```erlang
gen_server:call({via, global, Name}, Req).  % global uses the same API as user-defined registries
```

You can also call named processes on remote nodes (or on the local node):

```erlang
gen_server:call({Name, LocalNode}, Req).   % local by name -- uses whereis(Name)
gen_server:call({Name, RemoteNode}, Req).   % remote by name -- uses erlang:send({Name, RemoteNode}, ...)
```
