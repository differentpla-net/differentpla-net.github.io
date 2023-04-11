---
title: "Erlang ssh shell replacement"
date: 2023-03-27T18:44:00Z
tags: erlang
---

Motivation here is to avoid needing anything like `shelly` -- instead just have a sidecar with the SSH daemon that fires
up a remote console.

That is: we can have an SSH daemon in the app; see the erlang-cluster series. To log/audit use, we need an ssh proxy, which is also fine.

But: it means complications. If, instead, we could avoid touching the main Erlang app and have a sidecar which also did logging, that'd be neater.

The Erlang ssh daemon accepts a `shell` option, which defaults to `shell:start/1`. Can it be replaced?

ssh_cli.erl -> hamdle_ssh_msg({ssh_cm, ..., {shell, ...}}) ->
  start_shell

Converts the `shell` option to something that can be passed to `group:start`, which is part of the user_drv shenanigans; I think Ferd wrote something about it.
