---
title: "Sending (some) functions between OTP versions fails with 'badfun'"
date: 2025-04-29T11:58Z
tags: erlang
---

Did you know that you can send Erlang functions between Erlang nodes? You can. Did you know that the OTP versions need
to match?

## Start Erlang distribution

We'll start two Erlang nodes, running different major versions of OTP:

```
$ source ~/.kerl/erlangs/OTP-26.2.5.3/activate
$ erl -sname otp26

Erlang/OTP 26 [erts-14.2.5.3] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit]

Eshell V14.2.5.3 (press Ctrl+G to abort, type help(). for help)
(otp26@roger-m1)1>
```

```
$ source ~/.kerl/erlangs/OTP-27.3.3/activate
$ erl -sname otp27

Erlang/OTP 27 [erts-15.2.6] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit]

Eshell V15.2.6 (press Ctrl+G to abort, type help(). for help)
(otp27@roger-m1)1>
```

Then we'll connect them into a cluster; run this on the OTP-26 node:

```
(otp26@roger-m1)1> net_kernel:connect_node('otp27@roger-m1').
true
(otp26@roger-m1)2> nodes().
['otp27@roger-m1']
```

## Register shell processes

To communicate between the two nodes, we'll need a registered process; we'll register the shell:

```
(otp26@roger-m1)3> register(shell, self()).
true
```

```
(otp27@roger-m1)1> register(shell, self()).
true
```

We don't need to register both, since we'll only send messages in one direction, but I like the symmetry.

## Send a simple message

```
(otp26@roger-m1)5> {shell, 'otp27@roger-m1'} ! hello.
hello
```

```
(otp27@roger-m1)3> flush().
Shell got hello
ok
```

That works.

## Send a simple function

Now to send a function:

```
(otp26@roger-m1)7> {shell, 'otp27@roger-m1'} ! fun() -> hello end.
#Fun<erl_eval.43.105768164>
```

```
(otp27@roger-m1)3> F = receive F -> F end.
#Fun<erl_eval.43.105768164>
(otp27@roger-m1)4> F().
hello
```

## Send a more complex function

That works. What if we send something slightly more complicated?

```
(otp26@roger-m1)6> {shell, 'otp27@roger-m1'} ! fun(M) -> maps:size(M) end.
#Fun<erl_eval.42.105768164>
```

```
(otp27@roger-m1)5> G = receive G -> G end.
#Fun<erl_eval.42.105768164>
(otp27@roger-m1)6> G(#{}).
** exception error: bad function #Fun<shell.5.18354929>
```

It fails with "bad function" (or `badfun`), because the serialized function depends on OTP-26, and we're running OTP-27.
