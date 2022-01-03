---
title: "IP-based pod A records"
date: 2022-01-03T11:11:00Z
tags: kubernetes
---

A while ago, I asked [why pod names don't resolve in
DNS](https://stackoverflow.com/questions/60741801/why-arent-pod-names-registered-in-kubernetes-dns), and never really
got a satisfactory answer. One way you can connect to a pod (rather than with a service), is to use the dashed-IP form
of the pod address, e.g. `10-42-2-46.default.pod.cluster.local`. Here's how it works.

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

## References

- <https://www.digitalocean.com/community/tutorials/an-introduction-to-the-kubernetes-dns-service>
- <https://docs.aws.amazon.com/eks/latest/userguide/managing-coredns.html>
- <https://coredns.io/plugins/kubernetes/>
