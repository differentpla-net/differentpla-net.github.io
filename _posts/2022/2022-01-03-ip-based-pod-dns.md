---
title: "IP-based pod A records"
date: 2022-01-03T11:11:00Z
tags: kubernetes
---

A while ago, I asked [why pod names don't resolve in
DNS](https://stackoverflow.com/questions/60741801/why-arent-pod-names-registered-in-kubernetes-dns), and never really
got a satisfactory answer. One way you can connect to a pod (rather than with a service), is to use the dashed-IP form
of the pod address, e.g. `10-42-2-46.default.pod.cluster.local`. As I noted in that Stack Overflow question, this is a
hack. Here's how it works.

## Background

I'm looking at setting up an Erlang/Elixir cluster in my Kubernetes cluster, using
[libcluster](https://github.com/bitwalker/libcluster), and I'm trying to get my head around some of the implied
constraints. For example, if I want to use the [`Cluster.Strategy.Kubernetes`
strategy](https://hexdocs.pm/libcluster/Cluster.Strategy.Kubernetes.html), in `:dns` mode, it requires that my Erlang
node is named with the dashed-IP pod address, using something like this:

```bash
# MY_POD_IP, MY_POD_NAMESPACE are injected.
# See https://kubernetes.io/docs/tasks/inject-data-application/environment-variable-expose-pod-information/
POD_A_RECORD=$(echo $MY_POD_IP | sed 's/./-/g')
CLUSTER_DOMAIN="cluster.local"    # default; correct value left as an exercise for the reader.
erl -name app@"${POD_A_RECORD}.${MY_POD_NAMESPACE}.pod.${CLUSTER_DOMAIN}"
```

This means that my node (Erlang node, not k8s node) is named `app@10-42-2-46.default.pod.cluster.local`, and when libcluster attempts to join the cluster, this is the name it will use when setting up Erlang distribution.

<div class="callout callout-info" markdown="span">
When an Erlang node (the "initiator") connects to another Erlang node (the "acceptor"), it uses the node name as part of setting up the session. The initiator's idea of the acceptor's node name MUST match the acceptor's idea of its own node name.
</div>

## It's a hack

You can use _any_ dashed-IP you want; it'll resolve to the dotted-IP. To show this, we'll first need a temporary container:

```
$ kubectl run --stdin --tty --rm alpine --image=alpine -- sh

# apk add iputils
...
```

And then we can ping an arbitrary dashed-IP address (`1.1.1.1` is [CloudFlare's DNS](https://www.cloudflare.com/en-gb/learning/dns/what-is-1.1.1.1/)):

```
# ping 1-1-1-1.default.pod.cluster.local
PING 1-1-1-1.default.pod.cluster.local (1.1.1.1) 56(84) bytes of data.
64 bytes from one.one.one.one (1.1.1.1): icmp_seq=1 ttl=58 time=8.36 ms
```

So how does that work? The default DNS for Kubernetes (since 1.11) uses CoreDNS, which uses plugins to define its operation. On k3s (and other Kubernetes implementations, presumably) you can get the list of plugins with the following:

```
kubectl --namespace kube-system get configmap coredns -o yaml
```

The relevant snippet is this:

```yaml
apiVersion: v1
data:
  Corefile: |
    .:53 {
        ...
        kubernetes cluster.local in-addr.arpa ip6.arpa {
          pods insecure
          ...
        }
        ...
```

The `kubernetes` plugin is documented [here](https://coredns.io/plugins/kubernetes/), and it talks about the `pods` directive:

> `pods` **POD-MODE** sets the mode for handling IP-based pod A records, e.g. `1-2-3-4.ns.pod.cluster.local. in A 1.2.3.4`. This option is provided to facilitate use of SSL certs when connecting directly to pods. Valid values for **POD-MODE**:
> - `disabled`: Default. Do not process pod requests, always returning `NXDOMAIN`
> - `insecure`: Always return an A record with IP from request (without checking k8s). This option is vulnerable to abuse if used maliciously in conjunction with wildcard SSL certs. This option is provided for backward compatibility with kube-dns.
> - `verified`: Return an A record if there exists a pod in same namespace with matching IP. This option requires substantially more memory than in insecure mode, since it will maintain a watch on all pods.

So, yeah, in `insecure` mode, it simply parses the dashed-IP and converts it to a dotted-IP.

<div class="callout callout-info" markdown="span">
Back to my original problem: how do I get the pod _name_ to resolve? ~~I could write a CoreDNS plugin that would do it correctly.~~ The `endpoint_pod_names` directive does this. I don't see Twilio's Platform/SRE team being too enthusiatic about that, though.<br/>So I guess I'll have to look into [subverting epmd](https://github.com/rlipscombe/epmd_docker). Again.
</div>

## References

- <https://www.digitalocean.com/community/tutorials/an-introduction-to-the-kubernetes-dns-service>
- <https://coredns.io/plugins/kubernetes/>

## Further Reading

- <https://lrascao.github.io/k8s-erlang-clustering/>
