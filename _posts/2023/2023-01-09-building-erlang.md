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
./otp_build configure --prefix="$ERL_TOP/install" && make -j$(nproc)
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
make install
```

...and you can use it from there:

```sh
export PATH="$ERL_TOP/install/bin:$PATH"
erl
```
