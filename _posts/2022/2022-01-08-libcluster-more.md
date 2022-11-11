---
title: "libcluster notes"
date: 2022-01-08T14:09:00Z
tags: elixir kubernetes libcluster
---

<div class="callout callout-info" markdown="span">
Back to my original problem: how do I get the pod _name_ to resolve? ~~I could write a CoreDNS plugin that would do it correctly.~~ The `endpoint_pod_names` directive does this. I don't see Twilio's Platform/SRE team being too enthusiastic about that, though.<br/>So I guess I'll have to look into [subverting epmd](https://github.com/rlipscombe/epmd_docker). Again.
</div>


Kubernetes provides a bunch of ways to discover other nodes:

- And if you're running multiple Erlang containers in the same pod, then you get contention with epmd. So maybe you deliberately run it as a sidecar, and _don't_ start it by default.
  - Did someone write that up somewhere?
- Your main problem, however, is that the node must be reachable via its own idea of its name -- TODO: explain why. TODO: See if TLS makes a difference.
  - which is where all the MY_POD_IP crap comes from.
- Then need to link to the epmd_docker subversion stuff, which I've got another k8s-ish instance of.

TODO: cluster-demo should actually _have_ exposed endpoints. What is the difference between
an endpoint and a service?

_Note: maybe we should be using endpoint slices?_


# ClusterDemo

## kubectl exec

```
$ kubectl exec --tty --stdin cluster-demo-59846d7d-8kth7 -- rel/cluster_demo/bin/cluster_demo remote

iex(cluster_demo@cluster-demo-59846d7d-8kth7)1> Node.list()
[]
```

Now, it might be possible to subvert epmd -- https://github.com/rlipscombe/epmd_docker -- such that enumerating the pods (nice names) could be converted to the dashed-IP pod name (ugly names) while the target node keeps its nice name.

I wonder whether it's possible to do a PR for Erlang that removes the need for the two names to match? It might be tricky, 'cos it uses the name to establish the shared secret, iirc. Or maybe TLS distribution could remove that need? Actually, given TLS distribution, you need some way to generate certificates on the fly, which is ... ugly ...?

I've got an Erlang (actually Elixir) node running. That means that there's a beam process and an epmd process in the container. So: I can enumerate the pod by using:

```
$ kubectl get pods -l app.kubernetes.io/name=cluster-demo -o json | jq -r '.items[0].status.podIP'    # using jq
10.42.2.46

$ kubectl get pods -l app.kubernetes.io/name=cluster-demo -o jsonpath='{range .items[*]}{.status.podIP}{"\n"}{end}'   # using jsonpath
10.42.2.46
```

The 10.42.2.46 address for the pod is just a pod address. It's not a ClusterIP or NodePort (or LoadBalancer) address, because I have no service exposed (yet). Why would I use a headless service, rather than the pod IP? Why is a service "headless"?

It's on rpi405, which _might_ be important...

```
$ kubectl get pods -l app.kubernetes.io/name=cluster-demo -o json | jq -r '.items[0].spec.nodeName'
rpi405
```

And with that, I can connect to epmd, I think:

```
1> erl_epmd:names("10.42.2.46").
{ok,[{"cluster_demo",45183}]}
```

But: I can't connect to the node using that IP address, because it needs the actual name. But: can subvert epmd...

Can expose pod name via environment variable; see https://kubernetes.io/docs/tasks/inject-data-application/environment-variable-expose-pod-information/, but it's only the pod name, not the long name. Also the namespace.

Can get the cluster.local part:

```
iex(cluster_demo@cluster-demo-566c999cd-2mfd5)3> :inet_res.getbyname('kubernetes.default.svc', :a)
{:ok,
 {:hostent, 'kubernetes.default.svc.cluster.local', [], :inet, 4,
  [{10, 43, 0, 1}]}}
```

The part after our original query, `kubernetes.default.svc` -- `.cluster.local`, is the cluster domain.

Service account name is in the JWT token, or you can expose it (`spec.serviceAccountName`) via the downward API.

We actually _have_ the IP address at this point, so it seems kinda daft to look it up again.

What we could do instead is to have the above query run in our replacement epmd process, and it can cache the IP address results for later.

The fact that the discovery _and_ connection/disconnection stuff in `libcluster` is coupled is annoying to me.

In https://hexdocs.pm/libcluster/Cluster.Strategy.Kubernetes.html#content:

> It assumes that all nodes share a base name

This makes it _awkward_ to have a heterogeneous cluster. Awkward, not impossible. The problem being that all nodes in
the cluster MUST have the same base name as in configuration, which means that you won't be able to differentiate
between them. You can't have, I don't know, `api@`, `queue@` and `worker@` nodes in the same cluster. At least, not
while using libcluster as intended.

Further, it means that you won't be able to have multiple nodes in a pod, or at least that it won't bother joining them
to the cluster, unless they're already joined. It uses `epmd` to discover the port for the service it _is_ connecting
to, but it'll ignore other nodes in the pod.

On the bright side, you can go epmdless, because libcluster allows you to swap out the connect/disconnect functions. Or
not. Maybe that's not what that's for. That's for implementations that don't want to use Erlang distribution at all.
TODO: Clarify this.

You can look up your own name happily, 'cos it's in `/etc/hosts`, and the default for Erlang is to look there. But you can't look up other names.

```
iex(cluster_demo@cluster-demo-587859c9b4-2dfzf)34> :inet.gethostbyname('cluster-demo-587859c9b4-2dfzf')
{:ok,
 {:hostent, 'cluster-demo-587859c9b4-2dfzf', ['cluster-demo-587859c9b4-2dfzf'],
  :inet, 4, [{10, 42, 2, 99}]}}
iex(cluster_demo@cluster-demo-587859c9b4-2dfzf)35> :inet.gethostbyname('cluster-demo-587859c9b4-sjlw7')
{:error, :nxdomain}
```

You still need to expose a service for `endpoint_pod_names` to work:

```
iex(cluster_demo@cluster-demo-587859c9b4-2dfzf)47> :inet.gethostbyname('cluster-demo-587859c9b4-2dfzf.cluster-demo.cluster-demo.svc.cluster.local')
{:ok,
 {:hostent,
  'cluster-demo-587859c9b4-2dfzf.cluster-demo.cluster-demo.svc.cluster.local',
  ['cluster-demo-587859c9b4-2dfzf.cluster-demo.cluster-demo.svc.cluster.local'],
  :inet, 4, [{10, 42, 2, 99}]}}
```

The duplication is because the service name and the namespace are both "cluster-demo".

Pods are namespaced. So if you wanted to subvert epmd (or write a CoreDNS plugin), and modelling it after
`1-1-1-1.default.pod.cluster.local`, you'd need something like this:

```elixir
:"node@pod-name-DEPLOY-SUFFIX.NAMESPACE.pod.cluster.local"
```

Could use `dnsConfig` to make that implicit...? See https://kubernetes.io/docs/concepts/services-networking/dns-pod-service/#pod-dns-config.

By which I mean that you could:
1. have node names of `node@pod-name-DEPLOY-SUFFIX`
2. use `dnsConfig` to add `search NAMESPACE.pod.cluster.local`
3. CoreDNS plugin to implement `POD.NS.pod.cluster.local` searches (in addition to the dotted-IP ones).

But you're never going to get ops to agree to that.

Oh, here's an idea:
1. node names as above.
2. `dnsConfig` also points to a sidecar DNS, which...
3. does the pod lookups. It'll need to defer to the default later, though.

Or an epmd module:

It needs configuration:

- If I'm looking up POD, it needs to know NS (it can get the current NS from the serviceaccount directory), and it needs
  to do a Kubernetes query to find the IP address.
- If I'm looking up POD.NS, we're good.

Or you just give up and use the headless service or dotted-IP modes.

`subdomain` gives:

```elixir
iex(cluster_demo@cluster-demo-559f5454df-v8wrd.cluster-demo.cluster-demo.svc.cluster.local)1> :inet.gethostbyname('cluster-demo-559f5454df-v8wrd.cluster-demo.cluster-demo.svc.cluster.local')
{:ok,
 {:hostent,
  'cluster-demo-559f5454df-v8wrd.cluster-demo.cluster-demo.svc.cluster.local',
  ['cluster-demo-559f5454df-v8wrd.cluster-demo.cluster-demo.svc.cluster.local'],
  :inet, 4, [{10, 42, 3, 109}]}}

iex(cluster_demo@cluster-demo-559f5454df-v8wrd.cluster-demo.cluster-demo.svc.cluster.local)2> :inet.gethostbyname('cluster-demo-559f5454df-w4q4b.cluster-demo.cluster-demo.svc.cluster.local')
{:ok,
 {:hostent,
  'cluster-demo-559f5454df-w4q4b.cluster-demo.cluster-demo.svc.cluster.local',
  ['cluster-demo-559f5454df-w4q4b.cluster-demo.cluster-demo.svc.cluster.local'],
  :inet, 4, [{10, 42, 1, 91}]}}
```

...but libcluster doesn't have a way to build hostnames like that (duplicate `cluster-demo`; service and namespace). It
_might_ have worked in `hostname` mode, but that requires a `StatefulSet`. This is annoying.
