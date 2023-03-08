---
title: "Loading .erlang from the working directory"
date: 2023-03-07T16:04:00Z
tags: erlang
---

In the [previous post]({% post_url 2023/2023-03-04-erlang-shell-prompt %}), I wrote about customizing the Erlang shell
prompt by using the `.erlang` file. Erlang looks for this file in `$HOME` (I'm simplifying here); it doesn't look in the
current working directory. How can we fix that?

I like the way that Elixir looks for an `.iex.exs` file in the current directory. But Erlang doesn't look for the
`.erlang` file in the current directory. The [documentation](https://www.erlang.org/doc/man/erl.html#configuration)
says:

> When Erlang/OTP is started, the system searches for a file named `.erlang` in the user's home directory and then
> `filename:basedir(user_config, "erlang")`.

On Linux, that usually resolves to `$HOME/.config/erlang`.

You can tell the shell to load another `.erlang` file by using the `c:erlangrc/1` function; it takes a list of
directories to look in. For example:

```erlang
c:erlangrc(["."]).
```

We can do that when starting `erl` with:

```sh
erl -run c erlangrc .
```

_(Thanks to Ruan for the tip)_

How can we do that automatically?

My first thought was something like this:

```sh
ERL_ZFLAGS="-run c erlangrc ." erl
```

...which works fine. You could set `ERL_ZFLAGS` in your profile, even. As far as I can tell, it doesn't break anything.
It even loads the default `.erlang` file from `$HOME` first.

So we're done, right? No.

<div class="callout callout-warning" markdown="span">
Loading arbitrary scripts from untrusted sources is a bad idea. Don't do that.
</div>

## direnv

If you're using [direnv and kerl]({% post_url 2014/2014-09-30-kerl-and-direnv %}), you could put it in your `.envrc`:

```sh
use erlang OTP-25.2.1
export ERL_ZFLAGS="-run c erlangrc ."
```

Because direnv prompts you to approve each `.envrc` file, we can use this to prevent automatically running `.erlang`
until we've checked it.

## erlang.mk

If you're using `erlang.mk`, you could use the `SHELL_OPTS` variable:

```makefile
SHELL_OPTS = -run c erlangrc .
```

## rebar3

I couldn't find a clean way to have this work in `rebar3 shell`. Look at [this Github
issue](https://github.com/erlang/rebar3/issues/2425) for inspiration.

## See also

- <https://azunyanmoe.wordpress.com/2011/04/05/the-erlangrc-file/>
