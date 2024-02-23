---
title: "CoreDNS Customization"
date: 2022-02-25T08:47:00Z
tags: k3s core-dns dns
layout: series
series: k3s
---

While [debugging pod DNS problems]({% post_url 2022/2022-02-25-pod-dns-problems %}), I discovered that CoreDNS allows
customization by importing extra zone files from a config map. I'm going to use that to forward queries for `k3s.differentpla.net` to my [custom CoreDNS instance]({% post_url 2021/2021-12-29-coredns %}).

## coredns-custom.yaml

```yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: coredns-custom
  namespace: kube-system
data:
  k3s.differentpla.net.server: |
    k3s.differentpla.net {
      forward . 192.168.28.181:32053
    }
```

The upstream specified in `forward` MUST be an IP address, otherwise you'll see something like the following in the
logs:

```
[ERROR] Restart failed: plugin/forward: not an IP address or file: "rpi401:32053"
```

## Apply it

```
$ kubectl apply -f coredns-custom.yaml
configmap/coredns-custom created
```

## Test it

```
$ kubectl run dnsutils -it \
  --restart=Never --rm \
  --image=registry.k8s.io/e2e-test-images/jessie-dnsutils:1.3 -- /bin/bash
```

```
root@dnsutils:/# nslookup kubernetes.default
Server:		10.43.0.10
Address:	10.43.0.10#53

Name:	kubernetes.default.svc.cluster.local
Address: 10.43.0.1
```

```
root@dnsutils:/# nslookup git.k3s.differentpla.net
Server:		10.43.0.10
Address:	10.43.0.10#53

Name:	git.k3s.differentpla.net
Address: 192.168.28.13
```

```
root@dnsutils:/# nslookup www.slashdot.org  # showing my age here
Server:		10.43.0.10
Address:	10.43.0.10#53

Non-authoritative answer:
Name:	www.slashdot.org
Address: 204.68.111.106
```

It works.

Well, DNS works. If you'll recall, my problem was that my ArgoCD installation couldn't talk to my Gitea installation. It
still can't, but now the problem is "x509: certificate signed by unknown authority". It's always DNS, except when it's
certificates. That's a problem for a later post.
