---
title: "Erlang SSH - No host key available"
date: 2023-01-08T16:31:00Z
tags: erlang ssh
---

You're experimenting with Erlang's [built-in SSH daemon](https://www.erlang.org/doc/man/ssh.html), and it fails with "No
host key available". What's up with that?

```erlang
1> ssh:daemon(10022).
{error,"No host key available"}
```

**tl;dr:** you don't have permission to read the SSH host keys or you don't have any SSH host keys (you do have an SSH
server installed, right?)

But, just in case you're interested, here's how I figured out the cause. There's a bit of looking in the Erlang/OTP
source code, and a bit of using the `dbg` module.

Searching for that text in the source code takes you to [this line](https://github.com/erlang/otp/blob/OTP-25.2/lib/ssh/src/ssh_connection_handler.erl#L1488) in the Erlang/OTP source code. It's in this function:

```erlang
available_hkey_algorithms(server, Options) ->
    case [A || A <- available_hkey_algos(Options),
               is_usable_host_key(A, Options)] of
        [] ->
            error({shutdown, "No host key available"});
	Algs ->
	    [atom_to_list(A) || A<-Algs]
    end.
```

Can we get a little help from the debugger here? It looks like the interesting functions are:

- `available_hkey_algos` -- does it return any available host (I assume) key algorithms?
- `is_usable_host_key` -- are any of them usable?

```erlang
dbg:start().
dbg:tracer().
dbg:tpl(ssh_connection_handler, available_hkey_algos, [{'_', [], [{return_trace}]}]).
dbg:tpl(ssh_connection_handler, is_usable_host_key, [{'_', [], [{return_trace}]}]).
dbg:p(all, c).
ssh:daemon(10022).
```

There's a lot of output, but we can skim through it a bit:

```
(<0.81.0>) call ssh_connection_handler:available_hkey_algos(
    ...
)
(<0.81.0>) returned from ssh_connection_handler:available_hkey_algos/1 -> ['ecdsa-sha2-nistp384',
                                                                           'ecdsa-sha2-nistp521',
                                                                           'ecdsa-sha2-nistp256',
                                                                           'ssh-ed25519',
                                                                           'ssh-ed448',
                                                                           'rsa-sha2-256',
                                                                           'rsa-sha2-512']
```

OK, so there are available algorithms. Are they usable?

```
(<0.81.0>) call ssh_connection_handler:is_usable_host_key('ecdsa-sha2-nistp384',...)
(<0.81.0>) returned from ssh_connection_handler:is_usable_host_key/2 -> false
(<0.81.0>) call ssh_connection_handler:is_usable_host_key('ecdsa-sha2-nistp521',...)
(<0.81.0>) returned from ssh_connection_handler:is_usable_host_key/2 -> false
(<0.81.0>) call ssh_connection_handler:is_usable_host_key('ecdsa-sha2-nistp256',...)
(<0.81.0>) returned from ssh_connection_handler:is_usable_host_key/2 -> false
(<0.81.0>) call ssh_connection_handler:is_usable_host_key('ssh-ed25519',...)
(<0.81.0>) returned from ssh_connection_handler:is_usable_host_key/2 -> false
(<0.81.0>) call ssh_connection_handler:is_usable_host_key('ssh-ed448',...)
(<0.81.0>) returned from ssh_connection_handler:is_usable_host_key/2 -> false
(<0.81.0>) call ssh_connection_handler:is_usable_host_key('rsa-sha2-256',...)
(<0.81.0>) returned from ssh_connection_handler:is_usable_host_key/2 -> false
(<0.81.0>) call ssh_connection_handler:is_usable_host_key('rsa-sha2-512',...)
(<0.81.0>) returned from ssh_connection_handler:is_usable_host_key/2 -> false
{error,"No host key available"}
```

That's a big pile of "nope".

What makes a host key usable? `is_usable_host_key` [looks like](https://github.com/erlang/otp/blob/OTP-25.2/lib/ssh/src/ssh_connection_handler.erl#L1634) this:

```erlang
is_usable_host_key(Alg, Opts) ->
    try ssh_transport:get_host_key(Alg, Opts)
    of
        _PrivHostKey -> true
    catch
        _:_ -> false
    end.
```

`ssh_transport:get_host_key/2` is [here](https://github.com/erlang/otp/blob/OTP-25.2/lib/ssh/src/ssh_transport.erl#L913)
and looks like this:

```erlang
get_host_key(SignAlg, Opts) ->
    case call_KeyCb(host_key, [SignAlg], Opts) of
	{ok, PrivHostKey} ->
            %% Check the key - the KeyCb may be a buggy plugin
            case valid_key_sha_alg(private, PrivHostKey, SignAlg) of
                true -> PrivHostKey;
                false -> exit({error, bad_hostkey})
            end;
	Result ->
            exit({error, {Result, unsupported_key_type}})
    end.
```

It uses `call_KeyCb` to call the configured callback. Let's look there next:

```erlang
call_KeyCb(F, Args, Opts) ->
    {KeyCb,KeyCbOpts} = ?GET_OPT(key_cb, Opts),
    UserOpts = ?GET_OPT(key_cb_options, Opts),
    apply(KeyCb, F, Args ++ [[{key_cb_private,KeyCbOpts}|UserOpts]]).
```

What _is_ the configured callback (in `key_cb`)?

According to [the documentation](https://www.erlang.org/doc/man/ssh.html#type-key_cb_common_option), the default value
for `key_cb` is `ssh_file`. We can confirm that by looking at the debug trace from above. It's in the bit I ellided:

```
  key_cb => {ssh_file,[]},
```

Right, so what's going on in `ssh_file:host_key/2`? It's documented [here](https://www.erlang.org/doc/man/ssh_file.html#host_key-2), incidentally.

```erlang
dbg:start().
dbg:tracer().
dbg:tpl(ssh_file, host_key, [{'_', [], [{return_trace}]}]).
dbg:p(all, c).
ssh:daemon(10022).
```

The trace looks like this:

```
(<0.81.0>) call ssh_file:host_key('ecdsa-sha2-nistp384',[{key_cb_private,[]}])
(<0.81.0>) returned from ssh_file:host_key/2 -> {error,eacces}
(<0.81.0>) call ssh_file:host_key('ecdsa-sha2-nistp521',[{key_cb_private,[]}])
(<0.81.0>) returned from ssh_file:host_key/2 -> {error,eacces}
(<0.81.0>) call ssh_file:host_key('ecdsa-sha2-nistp256',[{key_cb_private,[]}])
(<0.81.0>) returned from ssh_file:host_key/2 -> {error,eacces}
(<0.81.0>) call ssh_file:host_key('ssh-ed25519',[{key_cb_private,[]}])
(<0.81.0>) returned from ssh_file:host_key/2 -> {error,eacces}
(<0.81.0>) call ssh_file:host_key('ssh-ed448',[{key_cb_private,[]}])
(<0.81.0>) returned from ssh_file:host_key/2 -> {error,enoent}
(<0.81.0>) call ssh_file:host_key('rsa-sha2-256',[{key_cb_private,[]}])
(<0.81.0>) returned from ssh_file:host_key/2 -> {error,eacces}
(<0.81.0>) call ssh_file:host_key('rsa-sha2-512',[{key_cb_private,[]}])
(<0.81.0>) returned from ssh_file:host_key/2 -> {error,eacces}
```

There's no usable host key because every attempt fails with `{error,eacces}`.

We can't read the host key files. If you look in `/etc/ssh`, you'll see that the various `ssh_host_ALGO_key`
files can only be read by `root`. Since they're _private_ keys, that makes sense.

So: either we need to edit the permissions to allow our Erlang app to read the files (don't do this), or we need to run
our app as root (probably also don't do this), or we need to
[generate our own set of host keys]({% post_url 2022/2022-11-01-erlang-ssh %}#generating-host-keys).

On the other hand, if every attempt fails with `{error,enoent}`, then you probably don't have an SSH server installed
_at all_, and you'll definitely need to generate your own host keys.
