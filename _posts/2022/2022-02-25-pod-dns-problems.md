---
title: "Pod DNS Problems"
date: 2022-02-25T08:42:00Z
tags: k3s core-dns dns
layout: series
series: k3s
---

I've got an [extra instance]({% post_url 2021/2021-12-29-coredns %}) of CoreDNS running in my cluster, serving
`*.k3s.differentpla.net`, with LoadBalancer and Ingress names registered in it, and it's working fine for queries _to_
the cluster. It's _not_ working fine for queries _inside_ the cluster. What's up with that?

{% include alerts/info.html content="It's DNS. It's always DNS." %}

## Motivation

While setting up an ArgoCD project, I set the Repository URL to `https://git.k3s.differentpla.net/USER/REPO.git`, but it
failed, complaining that the name didn't resolve. This threw me because it works fine from outside the cluster.

## Debugging DNS Resolution

See <https://kubernetes.io/docs/tasks/administer-cluster/dns-debugging-resolution/>.

```
$ kubectl run dnsutils --image=k8s.gcr.io/e2e-test-images/jessie-dnsutils:1.3 --command -- sleep 3600
pod/dnsutils created

$ kubectl get pod dnsutils      # ...repeat until it's 'Running'.
```

DNS queries:

```
$ kubectl exec -i -t dnsutils -- nslookup kubernetes.default
Server:		10.43.0.10
Address:	10.43.0.10#53

Name:	kubernetes.default.svc.cluster.local
Address: 10.43.0.1
```

<div class="callout callout-info" markdown="span">
`dig` (unlike `nslookup`) doesn't honour `search` directives in `/etc/resolv.conf`, so `dig kubernetes.default` won't work.
</div>

### k3s.differentpla.net?

```
$ kubectl exec -i -t dnsutils -- nslookup git.k3s.differentpla.net
Server:		10.43.0.10
Address:	10.43.0.10#53

** server can't find git.k3s.differentpla.net: NXDOMAIN

command terminated with exit code 1
```

So, yeah, DNS lookup of `*.k3s.differentpla.net` from inside a pod isn't working.

Do external names resolve? I'd probably have noticed if they didn't, but let's check:

```
$ kubectl exec -i -t dnsutils -- nslookup blog.differentpla.net
Server:		10.43.0.10
Address:	10.43.0.10#53

Non-authoritative answer:
blog.differentpla.net	canonical name = differentpla-net.github.io.
Name:	differentpla-net.github.io
Address: 185.199.110.153
...etc.
```

It also works if I specify my router explicitly:

```
$ kubectl exec -i -t dnsutils -- nslookup git.k3s.differentpla.net 192.168.28.1
Server:		192.168.28.1
Address:	192.168.28.1#53

Name:	git.k3s.differentpla.net
Address: 192.168.28.13
```

So the problem is -- probably -- that the cluster-default CoreDNS (at `10.43.0.10`) isn't using my router (at `192.168.28.1`) for DNS resolution.

## Pod's DNS Policy

The default [DNS policy for a pod](https://kubernetes.io/docs/concepts/services-networking/dns-pod-service/#pod-s-dns-policy) is `"ClusterFirst"`. This forwards queries to the upstream nameserver inherited from the node.

I'm not convinced by that, however, because when I look at `/etc/resolv.conf` in a container, it has `nameserver
10.43.0.10`, which is the ClusterIP service for CoreDNS:

```
$ kubectl --namespace kube-system get service kube-dns
NAME       TYPE        CLUSTER-IP   EXTERNAL-IP   PORT(S)                  AGE
kube-dns   ClusterIP   10.43.0.10   <none>        53/UDP,53/TCP,9153/TCP   66d
```

## CoreDNS DNS Policy

The DNS Policy for CoreDNS is `"Default"` (which -- confusingly -- isn't the default). Anyhow:

```
$ kubectl --namespace kube-system get pod coredns-96cc4f57d-cztp4 -o yaml | grep dnsPolicy
  dnsPolicy: Default
```

The documentation says:

> `"Default"`: The Pod inherits the name resolution configuration from the node that the pods run on.

...which is odd, because that pod is running on `rpi405`:

```
$ kubectl --namespace kube-system get pod coredns-96cc4f57d-cztp4 -o json | jq -r '.spec.nodeName'
rpi405
```

...and DNS resolution works correctly on that node:

```
ubuntu@rpi405:~$ nslookup git.k3s.differentpla.net
Server:		127.0.0.53
Address:	127.0.0.53#53

Non-authoritative answer:
Name:	git.k3s.differentpla.net
Address: 192.168.28.13
```

So: is k3s doing something out-of-spec with CoreDNS? Or am I just misunderstanding how it's supposed to work?

## CoreDNS ConfigMap

```
$ kubectl --namespace kube-system get cm coredns -o yaml
...
data:
  Corefile: |
    .:53 {
        ...
        forward . /etc/resolv.conf
    }
...
```

Nothing particularly surprising there. What's in `/etc/resolv.conf` in the CoreDNS pod?

```
$ kubectl --namespace kube-system exec -it coredns-96cc4f57d-cztp4 -- /bin/sh
error: Internal error occurred: error executing command in container: failed to exec in container: failed to start exec "139317fa90a415ce57a358ea81920eca242b4f258ffd4adc7c02544415eb5a4c": OCI runtime exec failed: exec failed: container_linux.go:380: starting container process caused: exec: "/bin/sh": stat /bin/sh: no such file or directory: unknown
```

That's a problem: The CoreDNS container is so stripped down that it doesn't even contain a shell.

## Debugging with an ephemeral debug container

See <https://kubernetes.io/docs/tasks/debug-application-cluster/debug-running-pod/>.

```
$ kubectl --namespace kube-system debug -it coredns-96cc4f57d-cztp4 --image=busybox
Defaulting debug container name to debugger-lfc7b.
error: ephemeral containers are disabled for this cluster (error from server: "the server could not find the requested resource").
```

Another problem: ephemeral containers are behind a feature gate, and k3s has it disabled by default.

I might have been able to work around this by editing the CoreDNS Deployment to explicitly add a debugging container,
but upon further reading, it seems that containers within a pod don't share a filesystem (unless you explicitly use a
Volume), which means that I'd not be able to inspect CoreDNS's `/etc/resolv.conf`, anyway.

## CoreDNS logging

See <https://kubernetes.io/docs/tasks/administer-cluster/dns-debugging-resolution/#are-dns-queries-being-received-processed>:

Turn on CoreDNS logging by editing the config map:

```
$ kubectl --namespace kube-system edit cm coredns
```

```yaml
apiVersion: v1
data:
  Corefile: |
    .:53 {
        log     # <--
        errors
        health
# ...
```

```
$ kubectl exec -i -t dnsutils -- nslookup git.k3s.differentpla.net
...

$ kubectl --namespace kube-system logs coredns-96cc4f57d-cztp4
...
[INFO] 127.0.0.1:55248 - 51746 "HINFO IN 7620827858334401234.2272423027405113866. udp 57 false 512" NXDOMAIN qr,rd,ra 132 0.012383467s
[INFO] 10.42.0.179:43855 - 12121 "A IN git.k3s.differentpla.net.default.svc.cluster.local. udp 68 false 512" NXDOMAIN qr,aa,rd 161 0.000657711s
[INFO] 10.42.0.179:49057 - 9870 "A IN git.k3s.differentpla.net.svc.cluster.local. udp 60 false 512" NXDOMAIN qr,aa,rd 153 0.000627026s
[INFO] 10.42.0.179:56370 - 40719 "A IN git.k3s.differentpla.net.cluster.local. udp 56 false 512" NXDOMAIN qr,aa,rd 149 0.000681747s
[INFO] 10.42.0.179:37850 - 36437 "A IN git.k3s.differentpla.net. udp 42 false 512" NXDOMAIN qr,rd,ra 138 0.103363513s
```

Nothing directly interesting in there. Turn logging off again.

### Aside: Custom overrides?

What _is_ interesting is that the log contains these:

```
[WARNING] No files matching import glob pattern: /etc/coredns/custom/*.server
```

It seems that, at some point in November 2021, support was [added](https://github.com/k3s-io/k3s/pull/4397) for
customizing the cluster CoreDNS server. That bears further investigation: maybe I can get rid of my custom instance of
CoreDNS (which would be cleaner), or maybe I can explicitly forward `k3s.differentpla.net` to it (which would fix the
problem at hand).

## Container filesystem

The `/etc/resolv.conf` file for the CoreDNS container must be stored _somewhere_. k3s uses `containerd`, so we can go
looking on the relevant node:

```
ubuntu@rpi405:~$ sudo ctr c ls | grep coredns
b472a447...    docker.io/rancher/mirrored-coredns-coredns:1.8.6              io.containerd.runc.v2
da61d860...    docker.io/rancher/mirrored-coredns-coredns:1.8.6              io.containerd.runc.v2
```

_Note:_ The long identifiers are truncated for readability in this section.

Why are there two of them? Dunno. There's only one container in the pod:

```
$ kubectl --namespace kube-system get pod coredns-96cc4f57d-cztp4 -o json | jq '.spec.containers | length'
1
```

Having said that, however:

```
$ kubectl --namespace kube-system get pod coredns-96cc4f57d-cztp4 -o json | gron | grep b472
json.status.containerStatuses[0].lastState.terminated.containerID = "containerd://b472a447...";

$ kubectl --namespace kube-system get pod coredns-96cc4f57d-cztp4 -o json | gron | grep da61
json.status.containerStatuses[0].containerID = "containerd://da61d860...";
```

So it looks like we want `da61...`. You'll note that `/etc/resolv.conf` is mounted explicitly:

```
ubuntu@rpi405:~$ sudo ctr c info da61d860... | gron | grep resolv
json.Spec.mounts[11].destination = "/etc/resolv.conf";
json.Spec.mounts[11].source = "/var/lib/rancher/k3s/agent/containerd/io.containerd.grpc.v1.cri/sandboxes/304b.../resolv.conf";
```

What's in it?

```
ubuntu@rpi405:~$ sudo cat /var/lib/rancher/k3s/agent/containerd/io.containerd.grpc.v1.cri/sandboxes/304b.../resolv.conf
nameserver 8.8.8.8
```

Well, _there's_ your problem.

CoreDNS, as default-configured by k3s, uses Google's DNS servers at `8.8.8.8`, rather than locally-configured DNS
servers. So it's asking `8.8.8.8` about `git.k3s.differentpla.net`, which is asking my externally-facing DNS server
about `git.k3s.differentpla.net`, which knows nothing about it, and returns `NXDOMAIN`.

## Rancher: Troubleshooting DNS

It turns out that the Rancher docs have "Troubleshooting DNS" page, with a section entitled [Check upstream nameservers
in resolv.conf][check-upstream-nameservers-in-resolv-conf]. It says to try this:

[check-upstream-nameservers-in-resolv-conf]: https://rancher.com/docs/rancher/v2.6/en/troubleshooting/dns/#check-upstream-nameservers-in-resolv-conf

```
$ kubectl run -i --restart=Never --rm test-${RANDOM} --image=ubuntu --overrides='{"kind":"Pod", "apiVersion":"v1", "spec": {"dnsPolicy":"Default"}}' -- sh -c 'cat /etc/resolv.conf'
nameserver 8.8.8.8
pod "test-19198" deleted
```

That just confirms it. But where is it coming from, and can I change it? _Should_ I change it?

## Whence 8.8.8.8?

Searching for `"8.8.8.8"` in the k3s repository on GitHub takes me to [commit a4df9f4](https://github.com/k3s-io/k3s/commit/a4df9f4ab147e66c96fd791fb52d3a93787f291c), wherein it "validates" the nameserver configured in the node's `/etc/resolv.conf`, and if that's not valid, uses `8.8.8.8` by default.

To check for validity, it uses the Go function `IsGlobalUnicast`, which is [implemented](https://cs.opensource.google/go/go/+/refs/tags/go1.17.7:src/net/ip.go;l=190) as follows:

```go
func (ip IP) IsGlobalUnicast() bool {
	return (len(ip) == IPv4len || len(ip) == IPv6len) &&
		!ip.Equal(IPv4bcast) &&
		!ip.IsUnspecified() &&
		!ip.IsLoopback() &&
		!ip.IsMulticast() &&
		!ip.IsLinkLocalUnicast()
}
```

The node's `/etc/resolv.conf` looks like this:

```
$ cat /etc/resolv.conf
# ...systemd warnings ellided...

nameserver 127.0.0.53
...
```

...and _that's_ a loopback address, so it thinks this file is invalid.

According to my reading of the source code in that commit, it ought to next try `/run/systemd/resolve/resolv.conf`,
which looks like this:

```
$ cat /run/systemd/resolve/resolv.conf
# ...systemd warnings ellided...

nameserver 192.168.28.1
nameserver fe80::...
...
```

That `nameserver 192.168.28.1` is my router, so it ought to be valid, but I think it's being tripped up by the
`nameserver fe80::...`, which is a link-local unicast address (even though it's _also_ my router), and that's causing it
to reject the whole file. At that point, it gives up and defaults to using `8.8.8.8`.

My router is set up for "Stateless" RA mode, so the `fe80::...` nameserver is apparently being invented by each client.

## Using a custom override

As noted above, CoreDNS supports importing custom zones by placing files in the `/etc/coredns/custom` directory. I'll
cover this in a [separate post]({% post_url 2022/2022-02-25-coredns-custom %}).
