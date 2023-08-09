---
title: "Building Erlang/OTP"
date: 2023-01-09T18:06:00Z
tags: erlang
---

There's more comprehensive information in various files in the `HOWTO` directory, but ain't nobody got time for that. This works for me.

## Building it

```sh
export LANG=C
export ERL_TOP="$(pwd)"
./otp_build configure
make -j$(nproc)
```

## Using it

You can sometimes get away with using it directly:

```sh
export PATH="$ERL_TOP/bin:$PATH"
erl
```

## Installing it

But, more often, you'll need to install it:

```sh
export RELEASE_ROOT="$HOME/OTP"
cd "$ERL_TOP"
make release
```

```sh
cd "$RELEASE_ROOT"
./Install -minimal "$RELEASE_ROOT"
```

...and you can use it from there:

```sh
export PATH="$RELEASE_ROOT/bin:$PATH"
erl
```

It will display something like the following:

```
Erlang/OTP 27 [DEVELOPMENT] [erts-14.0.2] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit]

Eshell V14.0.2 (press Ctrl+G to abort, type help(). for help)
1>
```
