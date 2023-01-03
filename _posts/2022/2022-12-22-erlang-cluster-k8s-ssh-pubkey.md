---
title: "Erlang cluster on Kubernetes: SSH public key authentication"
short_title: "SSH public key authentication"
date: 2022-12-22T15:25:00.000Z
layout: series
series: erlang-cluster-k8s
---

In the [previous post]({% post_url 2022/2022-12-22-erlang-cluster-k8s-ssh %}), I showed how to enable SSH access to the
Erlang remote console on a pod. When we left it, it had no authentication. Let's fix that.

## SSH daemon options

To enable public key authentication, we need to change `erlclu_app.erl` as follows:

```erlang
start_ssh_daemon(SystemDir, UserDir) when is_list(SystemDir), is_list(UserDir) ->
    {ok, _} = ssh:daemon(10022, [
        {system_dir, SystemDir},
        {user_dir, UserDir},
        {auth_methods, "publickey"}
    ]),
    ?LOG_INFO("SSH daemon listening on port 10022");
start_ssh_daemon(_SystemDir, _UserDir) ->
    ?LOG_WARNING("Not starting SSH daemon").
```

In particular, note the `{auth_methods, "publickey"}`. Unusually, this is a comma-separated string, rather than the list
of atoms that you might expect.

Since we'll be using a ConfigMap for `authorized_keys`, I took the opportunity to move the host key into a secret. To
make that more flexible, we set `SystemDir` and `UserDir` from environment variables:

```erlang
start_ssh_daemon() ->
    SystemDir = os:getenv("SSH_SYSTEM_DIR"),
    UserDir = os:getenv("SSH_USER_DIR"),
    start_ssh_daemon(SystemDir, UserDir).
```

## Setting environment variables

The environment variables need setting; we'll do that in the deployment:

```yaml
containers:
  - name: erlclu
    #...
    env:
      #...
      - name: SSH_SYSTEM_DIR
        value: /erlclu/ssh/system
      - name: SSH_USER_DIR
        value: /erlclu/ssh/user
```

## Host key secret

Previously, the host key was kept in the `priv/ssh/system` directory and created either manually, or as part of the build. We'll move it to a secret:

```
ssh-keygen -q -N "" -t rsa -f ssh_host_rsa_key
kubectl --namespace erlclu create secret generic ssh-host-key \
        --from-file=ssh_host_rsa_key=ssh_host_rsa_key \
        --from-file=ssh_host_rsa_key.pub=ssh_host_rsa_key.pub
```

This needs to be made available to the container:

```yaml
    volumeMounts:
      #...
      - name: ssh-host-key
        mountPath: /erlclu/ssh/system
        readOnly: true
volumes:
  - name: ssh-host-key
    secret:
      secretName: ssh-host-key
  #...
```

Note that there's no reason for us to write to the volume, so we use `readOnly: true`.

## Authorized Keys

To use publickey authentication, the SSH daemon expects to find an `authorized_keys` file; we'll use a ConfigMap for
this (because it only contains public keys, and isn't usually considered to be a secret).

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: authorized-keys-cm
data:
  authorized-keys: |
```

Initially, it's empty.

It's mounted in the container:

```yaml
    volumeMounts:
      #...
      - name: authorized-keys
        mountPath: /erlclu/ssh/user
        readOnly: true
volumes:
  #...
  - name: authorized-keys
    configMap:
      name: authorized-keys-cm
      items:
      - key: authorized-keys
        path: authorized_keys
```

## Adding an authorized key

If we attempt to log in at this point, it will fail, because we've not told it about the user's public key.

There are many ways we could manage the authorized keys. For example, we could create a custom resource `AuthorizedKey`
and have an operator reconcile these with the ConfigMap object. Or we could take advantage of the `key_cb` option to
`ssh:daemon/3` and avoid using the `authorized_keys` file entirely.

These are all overkill at the moment, however, so we'll just rely on editing the config map, as follows:

Get your public key:

```
cat ~/.ssh/id_rsa.pub
```

Add it to the ConfigMap:

```
kubectl --namespace erlclu edit configmap authorized-keys-cm
```

Edit the `data.authorized-keys` section. It looks something like this:

```
...
data:
  authorized-keys: |
    ssh-rsa AAAAB3.... user@host
...
```

Add the new public key and save the configmap. Wait for a few seconds until it's pushed to the pod, and then log in.

This still requires the `kubectl port-forward` command from earlier:

```
kubectl --namespace erlclu port-forward deployment/erlclu 10022:10022 &
ssh -p 10022 -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null localhost
```

But now it's more secure. Which is nice.
