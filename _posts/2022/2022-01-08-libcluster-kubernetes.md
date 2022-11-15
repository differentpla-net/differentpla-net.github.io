---
title: "libcluster and Kubernetes"
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
POD_A_RECORD=$(echo $MY_POD_IP | sed 's/\./-/g')
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

So, how does this all work? `libcluster` provides three different Kubernetes strategies:

- `Cluster.Strategy.Kubernetes`
- `Cluster.Strategy.Kubernetes.DNS`
- `Cluster.Strategy.Kubernetes.DNSSRV`

Let's dig into them and work out what they're doing and why.

## Cluster.Strategy.Kubernetes

First up: `Cluster.Strategy.Kubernetes`.

### Lookup Mode: Endpoints

```elixir
kubernetes_ip_lookup_mode: :endpoints
```

The default for the `Cluster.Strategy.Kubernetes` strategy is to look up endpoints. It does this by doing `GET /api/v1/namespaces/#{namespace}/endpoints?labelSelector=#{selector}`.

By default, `namespace` comes from the `/var/run/secrets/kubernetes.io/serviceaccount/namespace` file, but can be
overriden with the `:kubernetes_namespace` configuration entry. `selector` comes from the `:kubernetes_selector`
configuration entry. It should be something like `app=myapp` or `k8s-app=myapp` or `app.kubernetes.io/name=myapp`,
depending on which scheme you use for your labels.

As the libcluster documentation states, this is the equivalent of `kubectl get endpoints -l app=myapp`.

More specifically, it's the equivalent of this:

```
$ kubectl get endpoints -l app=myapp -o json | \
    jq '.items[].subsets[].addresses[] | {ip: .ip, hostname: .hostname, namespace: .targetRef.namespace}'
```

Yes, I could probably have figured out how to do that with `-o jsonpath`. I prefer `jq`.

It returns the IP address, the hostname and the namespace for each endpoint. The `hostname` field is only present if
you're using a `StatefulSet`. This will be relevant in a moment.

For this to work, your pod must be running as a service account with the ability to list endpoints.

### Lookup Mode: Pods


```elixir
kubernetes_ip_lookup_mode: :pods
```

With the `:pods` lookup mode, it does `GET /api/v1/namespaces/#{namespace}/pods?labelSelector=#{selector}`.

`namespace` and `selector` are the same as described earlier.

This is the equivalent of `kubectl get pods -l app=myapp`, or more specifically this:

```
$ kubectl get pods -l app=myapp -o json | \
  jq '.items[] | {ip: .status.podIP, namespace: .metadata.namespace}'
```

For this to work, your pod must be running as a service account with the ability to list pods.

libcluster doesn't capture the hostname, even though it's in `.metadata.name`. I suspect that this is an oversight.

### Target node names

libcluster then uses the results to create the target node name, depending on the `:mode` configuration setting, as
follows:

For `mode: :ip`, it uses `#{app_name}@#{ip}`. That is: it uses the IP address directly, e.g. `myapp@10.42.1.49`. Use the
following:

```bash
# rel/env.sh.eex
export RELEASE_NODE="myapp@${MY_POD_IP}"
```

For `mode: :hostname`, it uses `#{app_name}@#{hostname}.#{service_name}.#{namespace}.svc.#{cluster_name}.local`.

This requires a `StatefulSet`, otherwise (as described above), `hostname` is not set. Use the following:

```bash
# rel/env.sh.eex
MY_SERVICE_NAME="cluster-demo"    # :kubernetes_service_name from config, must match Service name.
CLUSTER_DOMAIN="cluster.local"    # correct value left as an exercise for the reader.
export RELEASE_NODE="myapp@$(hostname).${MY_SERVICE_NAME}.${MY_POD_NAMESPACE}.svc.${CLUSTER_DOMAIN}"
```

Alternatively, `hostname -f` returns the whole thing, so you can:

```bash
# rel/env.sh.eex
export RELEASE_NODE="myapp@$(hostname -f)"
```

For `mode: :dns`, it uses [IP-based pod A records]({% post_url 2022/2022-01-03-ip-based-pod-dns %}), e.g. `myapp@10-42-1-49.default.pod.cluster.local`. Use the following:

```bash
# rel/env.sh.eex
POD_A_RECORD=$(echo $MY_POD_IP | sed 's/\./-/g')
CLUSTER_DOMAIN="cluster.local"    # correct value left as an exercise for the reader.
export RELEASE_NODE="myapp@${POD_A_RECORD}.${MY_POD_NAMESPACE}.pod.${CLUSTER_DOMAIN}"
```

The `Cluster.Strategy.Kubernetes` strategy requires a service account with the ability to list endpoints or pods. If you
want to avoid that, you could use one of the DNS-based strategies instead. Read on for details.

## Cluster.Strategy.Kubernetes.DNS

This strategy requires a headless service. For our purposes, the difference is this: a normal service returns a single DNS result,
and a headless service returns multiple results. This means that you can either let Kubernetes choose a service for you,
or you can get all of them and choose your own:

```bash
/ # dig +short myapp.default.svc.cluster.local
10.43.136.13
/ # dig +short myapp-headless.default.svc.cluster.local
10.42.1.61
10.42.3.71
10.42.2.79
```

With this strategy, libcluster uses `#{app_name}@#{ip}`, the same as `mode: :ip`, above. Use the following:

```bash
# rel/env.sh.eex
export RELEASE_NODE="myapp@${MY_POD_IP}"
```

## Cluster.Strategy.Kubernetes.DNSSRV

This strategy does a DNS query for `SRV` records, rather than `A` records. It's the equivalent of the following:

```
dig +short -t SRV myapp.default.svc.cluster.local
```

An SRV query for a normal service returns exactly what you asked for:

```
# dig +short -t SRV normal-service.default.svc.cluster.local
0 100 80 normal-service.default.svc.cluster.local.
```

An SRV query for a headless service returns multiple entries, but the response depends on whether you're using a StatefulSet. Here's what it looks like if you're not:

```
# dig +short -t SRV headless-service.default.svc.cluster.local
0 33 80 10-42-3-71.headless-service.default.svc.cluster.local.
0 33 80 10-42-2-79.headless-service.default.svc.cluster.local.
0 33 80 10-42-1-61.headless-service.default.svc.cluster.local.
```

<div class="callout callout-info" markdown="span">
The numbers are a `(priority, weight, port)` tuple. Lower priorities should be tried first. Records with the same priority should be load-balanced according to the weight. The port is, well, the port (here port 80, HTTP).<br>
**Note:** If you don't specify any ports in the headless service spec, then it won't respond to `SRV` queries; it will respond to `A` queries.
</div>

If it's headless _and_ a StatefulSet, you get the following:

```
# dig +short -t SRV both.default.svc.cluster.local
0 33 80 both-1.both.default.svc.cluster.local.
0 33 80 both-0.both.default.svc.cluster.local.
0 33 80 both-2.both.default.svc.cluster.local.
```

<div class="callout callout-info" markdown="span">
`dig` (unlike `nslookup`) doesn't honour `search` directives in `/etc/resolv.conf`, so no `dig myservice.default` shortcuts in the above examples.
</div>

`Cluster.Strategy.Kubernetes.DNSSRV` assumes the latter: a headless StatefulSet, so you'll need to set your node name as follows:

```bash
# rel/env.sh.eex
export RELEASE_NODE="myapp@$(hostname -f)"
```

This is the same as for `Cluster.Strategy.Kubernetes` and `mode: :hostname`, as described above.

## Which one to use?

- If you can't run your pod under a custom service account, you will need to use one of the DNS-based options. You'll need a headless service.
  - If you're not using a StatefulSet, use `Cluster.Strategy.Kubernetes.DNS`. You'll have to use IP addresses.
  - If you're using a StatefulSet, use `Cluster.Strategy.Kubernetes.DNSSRV`.
- If you're prepared to use a custom service account, you can use `Cluster.Strategy.Kubernetes`.
  - If you're not using a StatefulSet, use `mode: :ip` (IP addresses) or `mode: :dns` (dashed IP pod names).
  - If you're using a StatefulSet, use `mode: :hostname`.
