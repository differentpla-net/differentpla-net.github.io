---
title: "Changing the Erlang shell prompt"
date: 2023-03-04T16:38:00Z
tags: erlang
---

Did you know that you can change the Erlang shell prompt?

I previously wrote about [colouring the 'iex' prompt]({% post_url 2022/2022-01-09-iex-prompt-colours %}). Can we do the
same for the Erlang shell prompt?

We need a module containing our new prompt function; we'll start with something basic, as follows:

```erlang
-module(shell_prompt).
-export([prompt_func/1]).

prompt_func(_Opts) ->
    "erl> ".
```

Compile it, and put the result in `$HOME/ebin`:

```sh
mkdir -p "$HOME/ebin"
erlc -o "$HOME/ebin" shell_prompt.erl
```

We need a `$HOME/.erlang` file:

```erlang
code:load_abs(filename:join([os:getenv("HOME"), "ebin", "shell_prompt"])).
shell:prompt_func({shell_prompt, prompt_func}).
```

The first line loads `$HOME/ebin/shell_prompt.beam`; the second line sets a function from that module as the prompt
function.

<div class="callout callout-warning" markdown="span">
The `.erlang` file is loaded on _every_ Erlang instantiation, including things like running Elixir's `mix` and so on.
Bear that in mind before you do anything too involved.
</div>

## Embedded mode

Why don't we allow loading arbitrary modules from `$HOME/ebin`? In that case, the first line would look like this
instead:

```erlang
% Doesn't work in "embedded" mode.
code:add_patha(filename:join([os:getenv("HOME"), "ebin"])).
```

Unfortunately, if you run an Erlang release (in "embedded" mode) -- `_build/prod/rel/foo/bin/foo console`, for example
-- it doesn't allow implicitly loading arbitrary modules, and you'll get `** exception error: undefined function
shell_prompt:prompt_func/1` every time it attempts to display a prompt.

## This BEAM file was compiled for a later version

If you compile `shell_prompt.erl` with, say, Erlang 26.x, and then attempt to load it into an Erlang 23.x session,
you'll get the following error message:

```
=ERROR REPORT==== 21-Sep-2023::14:30:11.433270 ===
Loading of .../ebin/shell_prompt.beam failed: badfile

=ERROR REPORT==== 21-Sep-2023::14:30:11.706057 ===
beam/beam_load.c(1886): Error loading module shell_prompt:
  This BEAM file was compiled for a later version of the run-time system than 23.
  To fix this, please recompile this module with an 23 compiler.
  (Use of opcode 172; this emulator supports only up to 170.)

** exception error: undefined function shell_prompt:prompt_func/1
* Bad prompt function: {shell_prompt,prompt_func}
```

You can fix the first problem by compiling the file with the oldest version of Erlang you're likely to use. It will
usually load into newer versions of Erlang without problem. I've tested with OTP-23.x and OTP-26.x.

If you're using `kerl`, that's something like this (depending on where you keep your Erlang installations):

```sh
~/.kerl/erlangs/OTP-23.3.4.19/bin/erlc -o "$HOME/ebin" shell_prompt.erl
```

You can _partially_ fix the first problem by checking the result of `code:load_abs`, but you still get the error report
the first time you attempt to load the file.

You can fix the second problem by checking that the function is exported. The following snippet does both:

```erlang
% Load the module explicity, otherwise it doesn't work in 'embedded' mode.
case code:load_abs(filename:join([os:getenv("HOME"), "ebin", "shell_prompt"])) of
    {module, _} ->
        case erlang:function_exported(shell_prompt, prompt_func, 1) of
            true ->
                shell:prompt_func({shell_prompt, prompt_func});
            _ ->
                skip
        end;
    _ ->
        skip
end.
```

## Shell Prompt

That results in a shell prompt that looks like this:

<pre class="dark"><code>Erlang/OTP 25 [erts-13.0.4] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit:ns]

Eshell V13.0.4  (abort with ^G)
erl&gt; &block;</code></pre>

We can make our prompt look identical to the default Erlang prompt with the following:

```erlang
prompt_func(Opts) ->
    prompt_func(erlang:is_alive(), lists:keyfind(history, 1, Opts)).

prompt_func(false, {history, N}) ->
    io_lib:format("~B> ", [N]);
prompt_func(true, {history, N}) ->
    io_lib:format("(~s)~B> ", [node(), N]).
```

From [the documentation](https://www.erlang.org/doc/man/shell.html#prompting):

> The function is called as `Mod:Func(L)`, where `L` is a list of key-value pairs created by the shell. Currently there
> is only one pair: `{history, N}`, where `N` is the current command number.

I decided to allow for the case where future Erlang adds more things to the list.

Aside: the default prompt implementation uses `~w` (format as an Erlang term) to display the history number. I'm not
sure why. I suspect it's just in case it's _not_ a number.

I like to also display the current process ID in the prompt:

```erlang
prompt_func(Opts) ->
    prompt_func(erlang:is_alive(), lists:keyfind(history, 1, Opts)).

prompt_func(false, {history, N}) ->
    io_lib:format("~w ~B> ", [self(), N]);
prompt_func(true, {history, N}) ->
    io_lib:format("~w (~s)~B> ", [self(), node(), N]).
```

That looks like this:

<pre class="dark"><code>$ erl -sname foo
Erlang/OTP 25 [erts-13.0.4] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit:ns]

Eshell V13.0.4  (abort with ^G)
<0.86.0> (foo@ROGER-SURFACEBOOK3)1&gt; &block;</code></pre>

## Colour

Let's add some colour:

```erlang
-define(Reset, "\e[0m").
-define(IRed, "\e[0;91m").
-define(IGreen, "\e[0;92m").
-define(IYellow, "\e[0;93m").
-define(IWhite, "\e[0;97m").

prompt_func(Opts) ->
    prompt_func(erlang:is_alive(), get_colour(), lists:keyfind(history, 1, Opts)).

prompt_func(false, _Colour, {history, N}) ->
    io_lib:format("~w ~B> ", [self(), N]);
prompt_func(true, Colour, {history, N}) ->
    io_lib:format("~w (" ++ Colour ++ "~s" ?Reset ")~B> ", [self(), node(), N]).

get_colour() ->
    get_colour(os:getenv("WHICH_ENVIRONMENT")).

get_colour(false) -> ?IWhite;
get_colour("dev") -> ?IGreen;
get_colour("test") -> ?IYellow;
get_colour(_) -> ?IRed.
```

That comes out looking like this:

<pre class="dark"><code>$ WHICH_ENVIRONMENT=dev erl -sname foo
Erlang/OTP 25 [erts-13.0.4] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit:ns]

Eshell V13.0.4  (abort with ^G)
<0.86.0> (<span style="color: #6f0">foo@ROGER-SURFACEBOOK3</span>)1&gt; &block;
</code></pre>

I had problems in the distant past (Erlang R16 or so) which made me go back to a monochrome prompt. Maybe I'll have
better luck this time.

## File location

Unlike Elixir and its `.iex.exs` file, Erlang doesn't look in the current directory for the `.erlang` file; per the
[documentation](https://www.erlang.org/doc/man/erl.html#configuration):

> When Erlang/OTP is started, the system searches for a file named `.erlang` in the user's home directory and then
> `filename:basedir(user_config, "erlang")`.

On Linux, that usually resolves to `$HOME/.config/erlang`.

It would occasionally be nice if it also looked in the current directory; I'll try to write another blog post about that
at some point.
