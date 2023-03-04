---
title: "Starting the Elixir shell from Ctrl+G"
date: 2023-03-04T15:59:00Z
tags: erlang elixir
---

The Erlang and Elixir shells have a "Job Control Mode" (confusingly referred to as "JCL") which allow you to start
multiple shells and switch between them.

You access the job control mode by pressing Ctrl+G. It looks like the following:

```
Erlang/OTP 25 [erts-13.0.4] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit:ns]

Eshell V13.0.4  (abort with ^G)
1>  (Ctrl+G)
User switch command
 -->
```

Type `?` followed by Enter for help:

```
 --> ?
  c [nn]            - connect to job
  i [nn]            - interrupt job
  k [nn]            - kill job
  j                 - list all jobs
  s [shell]         - start local shell
  r [node [shell]]  - start remote shell
  q                 - quit erlang
  ? | h             - this message
```

## TODO

- Demonstrate switching between shells.
- Gonna need a way to demonstrate that they're different shells.
  - prompt_function or whatever it's called.
- Demonstrate that Elixir's JCL starts the Erlang shell.
- Show how to start Elixir's shell.
- Show how to start a custom shell.
