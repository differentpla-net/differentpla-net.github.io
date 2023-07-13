---
title: "Remotely loading Erlang modules by copy-pasting"
date: 2023-07-13T13:59:00.000Z
tags: erlang
---

If you want to replace an Erlang module on a remote node, but you're unable to copy files to it, and you're unable to
make RPC calls to it, but you _can_ get a remote console, here's what you do:

Start a local erlang shell, then:

```erlang
Mod = foo.

% Load the module locally.
{module, Mod} = code:load_file(Mod).
{Mod, Bin, File} = code:get_object_code(Mod).
rp(Bin).  % Copy the output from this.
```

On the remote console:

```erlang
Mod = foo.
File = "foo.erl".   % from above.
Bin = <<...>>.  % Paste the output from above.
code:load_binary(Mod, File, Bin).
```
