---
title: "Writing a kubectl ssh plugin"
date: 2023-02-28T12:54:00Z
tags: kubernetes
---

In [this post]({% post_url 2022/2022-12-22-erlang-cluster-k8s-ssh %}), I showed how to access the Erlang console via SSH
using `kubectl port-forward`.

```sh
kubectl --namespace erlclu port-forward deployment/erlclu 10022:22 &
ssh -p 10022 -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null localhost
```

This works for any pod running an SSH daemon, incidentally.

But it's a bunch of typing (or copy-pasting). It would be nice if it was a single command. In fact, it would be awesome
if it was a kubectl plugin, so that we could run it as `kubectl ssh POD`. Here's how.

## Hello kubectl

To implement a kubectl plugin -- for example `kubectl hello`, you just put `kubectl-hello` somewhere in `$PATH`. It can be written in anything. Here's `kubectl-hello`:

```bash
#!/usr/bin/env bash
echo "Hello kubectl!"
```

```
$ kubectl hello
Hello kubectl!
```

## kubectl ssh

Create the `kubectl-ssh` script as follows:

```bash
#!/usr/bin/env bash

pod=$1      # actually anything that kubectl port-forward will accept.

exec 3< <(kubectl port-forward "$pod" 0:22)
read <&3 -r line

re='^Forwarding from .*:([0-9]+) -> 22$'
if [[ $line =~ $re ]]; then
    port="${BASH_REMATCH[1]}"
    ssh -p "$port" -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null localhost
else
    exit 1
fi
```

Mark it as executable and put it somewhere in `$PATH`.

Bugs:
- It only works with the default namespace, or that selected by `kubectl config set-context`; I'll figure that out
  later.

Of interest:

- You can pass anything that `kubectl port-forward` will accept, not just pods, so it'll work as `kubectl ssh
  deployment/whatever`, etc.
- It uses `0` as the local port; this causes `kubectl port-forward` to pick an arbitrary local port.
- We use redirection to fd 3 (and the following `read` from fd 3) to capture the chosen local port.
- Because ssh thinks we're connecting to `localhost`, it'll complain that the host key keeps changing, so we disable
  that with the `-o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null` options. This makes it less secure against
  MITM attacks, however.

## Tab completion

Since v1.26.0 (so you might need to upgrade), kubectl supports tab completion for plugins, and we can take advantage of
that. Create a `kubectl_complete-ssh` script as follows (and `chmod +x` and put it in `$PATH`):

```bash
#!/usr/bin/env bash

exec kubectl __complete port-forward "$@"
```

We take advantage of the fact that our tab completion is identical to that of `kubectl port-forward`, and that `kubectl`
has a hidden `__complete` command.
