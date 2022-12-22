---
title: "Erlang cluster on Kubernetes: SSH daemon"
short_title: "SSH daemon"
date: 2022-12-22T14:35:00.000Z
layout: series
series: erlang-cluster-k8s
tags: erlang kubernetes ssh
---

As mentioned [earlier]({% post_url 2022/2022-12-22-erlang-cluster-k8s-tls-distribution %}), using TLS for Erlang
distribution breaks `erlclu remote_console` (because it breaks `erl_call`). At the time, I worked around the problem by
using `nodetool`. This post shows how to use Erlang's SSH daemon instead.

Erlang has support for [running an SSH daemon](https://www.erlang.org/doc/man/ssh.html), exposing a remote console. I
wrote a post about that [here]({% post_url 2022/2022-11-01-erlang-ssh %}).

We start the daemon (in `erlclu_app.erl`) as follows:

```erlang
    SystemDir = filename:join([code:priv_dir(?APPLICATION), "ssh", "system"]),
    {ok, _} = ssh:daemon(10022, [
        {system_dir, SystemDir},
        {no_auth_needed, true}
    ]),
```

We have to create the `priv/ssh/system` directory and populate it with a host key:

```
mkdir -p priv/ssh/system
ssh-keygen -q -N "" -t rsa -f priv/ssh/system/ssh_host_rsa_key
```

This will get copied to the container when it's built, and we'll be able to use SSH to connect to the Erlang console:

```
kubectl --namespace erlclu port-forward deployment/erlclu 10022:10022 &
ssh -p 10022 -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null localhost
```

The `kubectl port-forward` command connects to an arbitrary pod in the deployment, and forwards port 10022 to it. The
second command uses SSH to connect. It turns off strict host key checking (because the host key will keep changing), and
disables the `known_hosts` file by directing it to `/dev/null`.

Note: The server completely disables authentication by using `no_auth_needed`. The [next blog post]({% post_url
2022/2022-12-22-erlang-cluster-k8s-ssh-pubkey %}) will talk about how to enable public key authentication; we'll fix it
then.
