---
title: "CoreDNS: endpoint_pod_names"
date: 2022-01-08T19:34:00Z
tags: elixir kubernetes libcluster
---

I'm still on the hunt for a way to connect Erlang nodes in a Kubernetes cluster by using pod names.

When you connect to an Erlang node, the name you use and the name it uses for itself must match. This means that no
matter how you discover the Erlang node, Erlang needs to be able to resolve that name in order to connect to the
destination node.

This is why libcluster requires a StatefulSet, or uses IP address-derived node names, or uses the actual IP address.
These are the only way you can refer to a pod inside Kubernetes.

## Why does this matter?

When you're not using a StatefulSet, your pod name is something like this:

```
$ hostname
cluster-demo-7bd795854f-298qd
```

...and your Erlang/Elixir node name is something like this:

```
$ ./cluster_demo/bin/cluster_demo remote
Erlang/OTP 24 [erts-12.2] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1]

Interactive Elixir (1.13.1) - press Ctrl+C to exit (type h() ENTER for help)
iex(cluster_demo@cluster-demo-7bd795854f-298qd)1>
```

It's subjective, but I prefer this to IP address-based names, for the following reasons:

- They're slightly more recognisable in logs. IP addresses are just (small) numbers, which are easily confused.
- In `cluster-demo-7bd795854f-298qd`, the `7bd795854f` part identifies the ReplicaSet, which might be important.
- IP addresses might be recycled, which is relevant if you're looking at historical logs.

But you're basically out of luck. Pod names are not resolvable in DNS.

## Or are you...?

The `endpoint_pod_names` setting for the `kubernetes` plugin in CoreDNS _might_ do what we want. The documentation says (slightly reformatted):

> `endpoint_pod_names` uses the pod name of the pod targeted by the endpoint as the endpoint name in A records, e.g.,
> `endpoint-name.my-service.namespace.svc.cluster.local. in A 1.2.3.4`
>
> By default, the endpoint-name name selection is as follows: Use the hostname of the endpoint, or if hostname is not
> set, use the dashed form of the endpoint IP address (e.g., `1-2-3-4.my-service.namespace.svc.cluster.local.`)
>
> If this directive is included, then name selection for endpoints changes as follows: Use the hostname of the endpoint,
> or if hostname is not set, use the pod name of the pod targeted by the endpoint. If there is no pod targeted by the
> endpoint or pod name is longer than 63, use the dashed IP address form.

This directive is not included by default, meaning that StatefulSet pods (which have a hostname) can be resolved as follows:

```
# dig +short web-0.web.default.svc.cluster.local
10.42.1.47
```

...and that other pods can be resolved as follows:

```
# dig +short 10-42-3-75.cluster-demo-headless.default.svc.cluster.local
10.42.3.75
```

But note that dashed-IP lookups _don't_ work for StatefulSet pods:

```
# dig +short 10-42-1-47.web.default.svc.cluster.local
(nothing)
```

## Enabling `endpoint_pod_names`

Run the following:

```
kubectl --namespace kube-system edit configmap coredns -o yaml
```

Add the `endpoint_pod_names` directive to the `kubernetes` plugin configuration, so that it looks something like this:

```
kubernetes cluster.local in-addr.arpa ip6.arpa {
  pods insecure
  endpoint_pod_names    # <-- add this
  fallthrough in-addr.arpa ip6.arpa
}
```

This will rewrite the CoreDNS configuration files, and CoreDNS will reload them.

## Try it out

```
# dig +short cluster-demo-7bd795854f-298qd.cluster-demo.default.svc.cluster.local
10.42.3.75
```

Seems to work.

## Caveats

- You might not be able to persuade your ops team to enable `endpoint_pod_names`.
  - It might not stay enabled when your cluster is upgraded.
- It looks like you need to redeploy your pods to get the pod names registered in DNS.
- Is it _really_ all that important that your Erlang node names look nice?
- You're going to need to set the node name to the long form anyway.
  - By default, k8s pods only have a short name, so `hostname -f` returns the same as `hostname`.
  - You can force long names by using a StatefulSet. We don't want to do this.
  - Alternatively, set the `subdomain` field in the Deployment.
- libcluster doesn't know anything about this, so you'll need to use something else.
  - That's not _that_ big a deal; libcluster's convenient, but not necessary.

## Alternatives

- You could run your own DNS resolver for Erlang node resolution.
  - You could run your own CoreDNS, giving you control over the plugin settings.
  - You could use <https://hex.pm/packages/dns>. This could perform Kubernetes API queries to look up pod names.
  - You'll need to persuade your Erlang nodes to use this DNS server in preference to the default. One of these ought to work:
    - Mess around with `/etc/resolv.conf`. You can use a ConfigMap to do this.
    - Use `dnsPolicy` and `dnsConfig`; see <https://kubernetes.io/docs/concepts/services-networking/dns-pod-service/#pod-dns-config>.
    - Read <https://www.erlang.org/doc/apps/erts/inet_cfg.html>
- You could [implement an alternative node discovery mechanism](https://www.erlang.org/doc/apps/erts/alt_disco.html).
  - See <https://github.com/rlipscombe/epmd_docker> for an example that works in Docker.
  - It should be relatively simple to make this use the Kubernetes API.

## Further Reading

- <https://lrascao.github.io/k8s-erlang-clustering/>, which I wish I'd discovered _before_ I started investigating this stuff.
