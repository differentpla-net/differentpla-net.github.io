---
title: "Erlang SSH"
date: 2022-11-01T10:25:00Z
tags: erlang ssh
---

Erlang/OTP provides a built-in SSH client and daemon. You can use this to expose the console directly over SSH.

```erlang
{ok, _} = application:ensure_all_started(ssh).
Port = 10022.
ssh:daemon(Port).
```

This will fail with `{error,"No host key available"}`, because -- unless you're running as root -- it can't read the
host keys from `/etc/ssh` (assuming sane defaults in your OS). Or you might not have an SSH server installed.

## Generating host keys

So you'll need some host keys. To generate an RSA key:

```bash
mkdir -p tmp/system
ssh-keygen -q -N "" -t rsa -f tmp/system/ssh_host_rsa_key
```

Then you can point the `system_dir` configuration option to that directory:

```erlang
{ok, _} = application:ensure_all_started(ssh).
Port = 10022.
ssh:daemon(Port, [{system_dir, "tmp/system"}]).
```

```
% ssh localhost -p 10022
The authenticity of host '[localhost]:10022 ([127.0.0.1]:10022)' can't be established.
RSA key fingerprint is SHA256:lYzafSjnu/28K4uI6iZn70NHX2Se3ovDAYltYRh5LG4.
Are you sure you want to continue connecting (yes/no/[fingerprint])? yes
Warning: Permanently added '[localhost]:10022' (RSA) to the list of known hosts.
SSH server
Enter password for "roger"
password:
```

## Password Authentication

You can't log in, because there's no password configured. We'll deal with that:

```erlang
{ok, _} = application:ensure_all_started(ssh).
Port = 10022.
ssh:daemon(Port, [{system_dir, "tmp/system"}, {password, "secret"}]).
```

```
% ssh localhost -p 10022
SSH server
Enter password for "roger"
password: <secret>
Eshell V13.1.1  (abort with ^G)
1>
```

...and we have an Erlang shell. To quit, type `exit().` and press Enter:

```
1> exit().
Connection to localhost closed.
```

## Usernames

Ideally we wouldn't use password authentication, but while we're here, we'll take a quick look at usernames. As it
stands, we've got a single password for all users. That's a bad practice:

- non-repudiation: everyone knows the password, so you can't prove that a particular user did (or didn't do) something.
  To be fair, there's not a lot of auditing going on inside the Erlang console, so this doesn't buy you _that_ much.
- revocation: everyone's using the same password, so if someone leaves, you have to change the password and tell
  everyone.

One option is to replace the `password` option with the `user_passwords` option:

```erlang
{ok, _} = application:ensure_all_started(ssh).
Port = 10022.
ssh:daemon(Port, [
    {system_dir, "tmp/system"},
    {user_passwords, [
        {"alice", "secret"},
        {"bob", "speakfriend"}
        ]}
    ]).
```

The docs say the following:

<div class="callout callout-danger" markdown="span">
Note that this is very insecure due to the plain-text passwords; it is intended for test purposes. Use the `pwdfun`
option to handle the password checking instead.
</div>

OK, let's do that:

```erlang
{ok, _} = application:ensure_all_started(ssh).
Port = 10022.
UserDb = [{"alice", "secret"}, {"bob", "speakfriend"}].
ssh:daemon(Port, [
    {system_dir, "tmp/system"},
    {pwdfun, fun(User, Password, _Peer, _State) -> lists:member({User, Password}, UserDb) end}
    ]).
```

In the above example, we've just used the same list of users, so that's not much more secure than passing the
`user_passwords` option, but it allows us to (e.g.) put the passwords in a file (probably hashed) or call out to an
external system to do the validation.

## Public Key Authentication

Passwords are bad. We'd prefer to use public key authentication. That's pretty easy, too:

```erlang
{ok, _} = application:ensure_all_started(ssh).
Port = 10022.
ssh:daemon(Port, [
    {system_dir, "tmp/system"},
    {user_dir, "tmp/user"},
    {auth_methods, "publickey"}
    ]).
```

We've specified `{auth_methods, "publickey"}` (which, for some reason, is a comma-separated string, rather than a list
of atoms).

By default, Erlang's SSH daemon looks in `~/.ssh/authorized_keys` for the list of allowed SSH users. You can change that
behaviour with the `user_dir` option, as shown above.

To add a user:

```
mkdir -p tmp/user
cat ~/.ssh/id_rsa.pub >> tmp/user/authorized_keys
```

Test it:

```
% ssh localhost -p 10022
Eshell V13.1.1  (abort with ^G)
1>
```

Looks good.

You still have all of the usual SSH problems of distributing public keys. I plan to look at that in a later post.

## Listing connected users

This isn't documented as a thing you can do on the daemon side of the connection, but the `ssh:connection_info/1,2`
functions take the PID of the connection, which you can discover in a few different ways:

```
5> inet:i().
Port Module   Recv Sent Owner     Local Address   Foreign Address State        Type
32   inet_tcp 0    0    <0.101.0> *:10022         *:*             ACCEPTING    STREAM
40   inet_tcp 3789 2853 <0.104.0> localhost:10022 localhost:53357 CONNECTED(O) STREAM
```

`<0.104.0>` is our SSH connection...

```
{% raw %}7> ssh:connection_info(pid(0,104,0), [peer, user]).
[{peer,{undefined,{{127,0,0,1},53357}}},
 {user,"roger"}]{% endraw %}
```

Alternatively you can get the connections and users like this:

```erlang
{% raw %}Daemons = [D || {{ssh_system_sup,_},D,supervisor,_} <- supervisor:which_children(sshd_sup)].
CSups = [S || D <- Daemons, {Id,S,_,_} <- supervisor:which_children(D), is_reference(Id)].
Connections = [C || S <- CSups, {connection,C,_,_} <- supervisor:which_children(S)].
Users = [{C, ssh:connection_info(C, [user])} || C <- Connections].{% endraw %}
```

## TODO

- Public key auth using custom behaviour (callback module?)
- SSH certificates; are they supported?
- SSH certificates; what if we used an ssh daemon with implicit jumping?

We could even use password callback for FIDO/TOTP validation.

