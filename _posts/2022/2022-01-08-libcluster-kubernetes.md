---
title: "libcluster: Cluster.Strategy.Kubernetes"
date: 2022-01-08T14:07:00Z
tags: elixir kubernetes libcluster
---

I'm looking at setting up an Erlang/Elixir cluster in my Kubernetes cluster, using
[libcluster](https://github.com/bitwalker/libcluster), and I'm trying to get my head around some of the implied
constraints.

For example, if I want to use the [`Cluster.Strategy.Kubernetes` strategy](https://hexdocs.pm/libcluster/Cluster.Strategy.Kubernetes.html),
in `:dns` mode, it requires that my Erlang node is named with the dashed-IP pod address, using something like this:

```bash
# MY_POD_IP, MY_POD_NAMESPACE are injected.
# See https://kubernetes.io/docs/tasks/inject-data-application/environment-variable-expose-pod-information/
POD_A_RECORD=$(echo $MY_POD_IP | sed 's/./-/g')
CLUSTER_DOMAIN="cluster.local"    # correct value left as an exercise for the reader.
erl -name app@"${POD_A_RECORD}.${MY_POD_NAMESPACE}.pod.${CLUSTER_DOMAIN}"
```

If you're using a recent Elixir version (1.10 or newer), which supports `mix release`, you should set the `RELEASE_NODE` environment variable. This is done in `rel/env.sh.eex`:

```bash
export RELEASE_DISTRIBUTION=name
export RELEASE_NODE="app@${POD_A_RECORD}.${MY_POD_NAMESPACE}.pod.${CLUSTER_DOMAIN}"
```

This means that my node (Erlang node, not k8s node) is named `app@10-42-2-46.default.pod.cluster.local`, and when
libcluster attempts to join the cluster, this is the name it will use when setting up Erlang distribution.

<div class="callout callout-info" markdown="span">
When an Erlang node (the "initiator") connects to another Erlang node (the "acceptor"), it uses the node name as
part of setting up the session. The initiator's idea of the acceptor's node name MUST match the acceptor's idea of its own node name.
</div>

So, how does this all work? `libcluster` provides three different Kubernetes strategies, each of which has multiple modes:

- `Cluster.Strategy.Kubernetes`
- `Cluster.Strategy.Kubernetes.DNS`
- `Cluster.Strategy.Kubernetes.DNSSRV`

Let's dig into them and work out what they're doing and why. First up: `Cluster.Strategy.Kubernetes`.

## Lookup Modes: Endpoints

```elixir
kubernetes_ip_lookup_mode: :endpoints
```

The default for the `Cluster.Strategy.Kubernetes` strategy is to look up endpoints.

It does this by doing `GET /api/v1/namespaces/#{namespace}/endpoints?labelSelector=#{selector}`.

By default, `namespace` comes from the `/var/run/secrets/kubernetes.io/serviceaccount/namespace` file, but can be
overriden with the `:kubernetes_namespace` configuration entry.

`selector` comes from the `:kubernetes_selector` configuration entry. It should be something like `app=myapp` or `k8s-app=myapp` or `app.kubernetes.io/name=myapp`, depending on which scheme you use for your labels.

For this to work, your pod must be running as a service account with the ability to list endpoints.

As the libcluster documentation states, this is the equivalent of `kubectl get endpoints -l app=myapp`.

More specifically, it's the equivalent of this:

```
$ kubectl get endpoints -l app=myapp -o json | \
    jq '.items[].subsets[].addresses[] | {ip: .ip, hostname: .hostname, namespace: .targetRef.namespace}'
```

Yes, I could probably have figured out how to do that with `-o jsonpath`. I prefer `jq`.

That is: it returns the IP address, the hostname and the namespace for each endpoint. The `hostname` field is only present if you're using a `StatefulSet`. This will be relevant in a moment.

## Lookup Modes: Pods


```elixir
kubernetes_ip_lookup_mode: :pods
```

With the `:pods` lookup mode, it does `GET /api/v1/namespaces/#{namespace}/pods?labelSelector=#{selector}`.

`namespace` and `selector` are the same as described earlier.

For this to work, your pod must be running as a service account with the ability to list pods.

This is the equivalent of `kubectl get pods -l app=myapp`.

More specifically, it's the equivalent of this:

```
$ kubectl get pods -l app=myapp -o json | \
  jq '.items[] | {ip: .status.podIP, namespace: .metadata.namespace}'
```

libcluster doesn't capture the hostname, even though it's in `.metadata.name`. I suspect that this is an oversight.

## Target node names

libcluster then uses the results to create the target node name, depending on the `:mode` configuration setting, as follows:

For `mode: :ip`, it uses `#{app_name}@#{ip}`. That is: it uses the IP address directly, e.g. `myapp@10.42.1.49`. Use the following:

```bash
# rel/env.sh.eex
export RELEASE_NODE="myapp@${MY_POD_IP}"
```

For `mode: :hostname`, it uses `#{app_name}@#{hostname}.#{service_name}.#{namespace}.svc.#{cluster_name}.local`.

This requires a `StatefulSet`, otherwise (as described above), `hostname` is not set.

Use the following:

```bash
# rel/env.sh.eex
MY_SERVICE_NAME="cluster-demo"    # :kubernetes_service_name from config, must match Service name.
CLUSTER_DOMAIN="cluster.local"    # correct value left as an exercise for the reader.
export RELEASE_NODE="myapp@$(hostname).${MY_SERVICE_NAME}.${MY_POD_NAMESPACE}.pod.${CLUSTER_DOMAIN}"
```

Alternatively, `hostname -f` returns the whole thing, so you can:

```bash
# rel/env.sh.eex
export RELEASE_NODE="myapp@$(hostname -f)"
```

For `mode: :dns`, it uses [IP-based pod A records]({% post_url 2022/2022-01-03-ip-based-pod-dns %}), e.g. `myapp@10-42-1-49.default.pod.cluster.local`. Use the following:

```bash
# rel/env.sh.eex
POD_A_RECORD=$(echo $MY_POD_IP | sed 's/./-/g')
CLUSTER_DOMAIN="cluster.local"    # correct value left as an exercise for the reader.
export RELEASE_NODE="myapp@${POD_A_RECORD}.${MY_POD_NAMESPACE}.pod.${CLUSTER_DOMAIN}"
```

That's enough for now; we'll deal with the other strategies in future posts.
