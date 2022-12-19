---
title: "How SSH proxy?"
date: 2022-12-19T15:43:00.000Z
---

Use cases:

## SSH to a kubernetes pod

The K8s pod exposes an SSH daemon. Rather than need `kubectl port-forward`, it would be nice to be able to `kubectl ssh deployment/my-app` or `kubectl ssh service/my-svc` or `kubectl ssh pod -l app=my-app` or whatever.

### SSH via SSM

Rather than `ssm jump` then `ssh IP`, it would be nice to be able to `ssm ssh IP`. It would be even nicer if `IP` (which is a K8s pod) could be discovered automatically.

Note that `ssm` is our wrapper for `aws session-manager`; I'm not sure how much applies here.

## Comparable things

- `assh`

## Investigation

https://github.com/moul/assh

How does `assh` do its thing? It does custom name resolution.

```
brew install assh
```

You need a `~/.ssh/assh.yml` file.

Essentially, it sets up your `~/.ssh/config` file and puts the following in it:

```
Host *
  ProxyCommand /.../assh connect --port=%p %h
```

It also puts a bunch of other comments (which might be actual directives?) in the config file. Or maybe it reads the assh.yml file?

It also puts Host entries in the config file, which might just be a convenience for tab completion.

## Idea

Don't actually _have_ to use the `ssh` command, though it does make things neater for (say) git, etc., which use `lib-ssh` which use the config file...?

If you set `ProxyCommand` to something that doesn't exist, it fails. I suspect that it's just expecting you to set up a connection and then to pass stdin/stdout over it.

This means that things like socat and wscat would probably work fine.

As far as I can tell, you can't use an environment variable to point to an alternative configuration file, which makes noodling around with stuff harder. You'd need symlinks or something. Is that even supported? You can use `-F`, though.

The ProxyCommand can use %h, %n, %p and %r; as defined here: https://man7.org/linux/man-pages/man5/ssh_config.5.html#TOKENS (for Linux, anyway; might be different for BSD).

So, what's a good way to demonstrate this working? Normally you use ssh as the ProxyCommand, with `ssh -o ProxyCommand="ssh -W %h:%p proxy" target`. That is: it initially connects to `proxy` using `ssh`, and forwards (`-W`) stdin/stdout to `%h:%p`, which are assumed to be another SSH daemon.

What is `target` used for in this command? Does it get passed to the other end?


Historically, ssh didn't have `-W`, which means we don't necessarily need to do that. How did that work?

```
ssh -tt user@proxy ssh -tt user@target
```

That is: ssh to proxy, then exec "ssh user@target". The `-tt` stuff forces TTY allocation.

This means that we should be able to see it in operation with an Erlang SSH daemon (merely because it lets us be an ssh daemon and fuck about with stuff).

Alternatively, just use `socat` or `nc`. That is:

```
ssh -o ProxyCommand="nc %h %p" foo
```

...will connect to %h:%p and will expect an SSH daemon to be running at the other end. There's almost certainly some docs on the internet explaining how that works. In fact: it's right there in the `ssh_config` man page.

Note that the `Host *` stuff is reapplied to the ProxyCommand if it happens to use `ssh`. So you can proxy through multiple hops if they're set up properly. Could get confusing, though.

`ssh:daemon` needs a host key; so we'll need:

```
mkdir -p tmp/system
ssh-keygen -q -N "" -t rsa -f tmp/system/ssh_host_rsa_key
```

```
{ok, _} = application:ensure_all_started(ssh).
ExecFun = fun(Cmd, User, Client) -> {ok, "hello world"} end.
{ok, _} = ssh:daemon(10022, [{system_dir, "tmp/system"}, {no_auth_needed, true}, {subsystems, []}, {shell, disabled}, {exec, {direct, ExecFun}}]).
```

```
ssh -p 10022 -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null localhost
```

`Prohibited` because `{shell, disabled}`.

```
% ssh -p 10022 -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null localhost hello
hello world
```

So we should be able to put that in `~/.ssh/config`:

```
Host erlang
  HostName localhost
  Port 10022
  StrictHostKeyChecking no
  UserKnownHostsFile /dev/null
```

...and then...

```
% ssh erlang hello
hello world
```

So: what happens if we use that as our proxy?

```
ssh -o ProxyCommand="ssh -W %h:%p erlang" foo
```

Forwarding is disabled. Because it is. We didn't use the `{tcpip_tunnel_in, true}` or `{tcpip_tunnel_out, true}` settings. I don't know which one is required. `in` apparently.

We don't need the exec stuff?

```
{ok, _} = application:ensure_all_started(ssh).
{ok, _} = ssh:daemon(10022, [
  {system_dir, "tmp/system"},
  {no_auth_needed, true},
  {subsystems, []},
  {shell, disabled},
  {exec, disabled},
  {tcpip_tunnel_in, true}]).
```

That attempts to forward, but fails with nxdomain, because `foo` ain't a thing. I'm still confused about what's going on here. I think:

`ssh -o ProxyCommand="ssh -W %h:%p erlang" foo` connects to the SSH daemon at `erlang` (which is our custom daemon; ignore that for now), and then asks it to open a tunnel to `%h:%p`. It then sends stdin/stdout over that tunnel. I don't know what `foo` is for? Maybe for server auth. Presumably I have have `foo` map to `bar` in the config file. Which one gets passed to the other end?

I can see what's actually getting _run_ with the following:

```
#!/bin/sh

>&2 echo "$*"
>&2 env | grep SSH | sort
exec ssh -F none $*
```

The `-F none` is to avoid looping if this proxy command is specified in the ssh config file. But it does break the
lookup for `erlang` which screws the alternate ports. Maybe there's an environment variable we can look at to see if
we're being used as a proxy? And how far we're nested.  Nope.

Because that looks like it'll cause a fork bomb, if we're not
careful.

```
ssh -o ProxyCommand="~/.ssh/bin/ssh-proxy -W %h:%p erlang" target
```

It's being run as `ssh -W target:22 erlang` -- that is: simply just set up a std/port forward.

OK, the Erlang implementation doesn't allow hooking the tunnel forwarding thing, so that's out. We need to look up the host to forward to before connecting.

That's not necessarily all bad. We can use a custom proxy command which converts the target definition before passing it to `ssh`.

Right, so next question: what is the `target` used for in the last bit?

Can I:

```
ssh -o ProxyCommand="ssh -W real-target:22 erlang" ignored-target
```

```
ssh -o ProxyCommand="ssh -F none -W balls:22 -p 10022 localhost -oStrictHostKeyChecking=false -oUserKnownHostsFile=/dev/null" ignored-target
```

Not a lot, afaict. Host key checking, maybe.

So, in theory, we should be able to use a ProxyCommand that, for kubernetes-looking hostnames, does the lookup.

Maybe...

```
ssh -o ProxyCommand="~/.ssh/bin/ssh-proxy -W %h:%p jump" pod/whatever
```

The `pod/whatever` gets passed as `%h`, including if it got munged in the config file, so we can either update the config file before running ssh -- can hosts in the file have `/` and stuff in them?

Or we can do a K8s query in the proxy and then `exec ssh -W pod-ip:22 jump`, where `jump` is our externally-facing SSH daemon.

That is:

1. Run an SSH daemon as a pod. Add an external service pointing to it. Could use a stock OpenSSH daemon, could use a custom (Erlang) one. Stock would be better, from a trust point of view.
2. Add some magic to the ssh config (see below).
3. Create the proxy command implementation.

```
Host pod/*
  ProxyCommand /Users/rogerlipscombe/.ssh/bin/kube-ssh-proxy %h "ssh -W %h:%p jump"
```

Namespaces could be a problem.
