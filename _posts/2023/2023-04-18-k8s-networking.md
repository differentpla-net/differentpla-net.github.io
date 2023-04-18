---
title: "Kubernetes Networking"
date: 2023-04-18T13:10:00.000Z
tags: kubernetes
---

My K3s cluster uses MetalLB as a bare-metal "load-balancer", and I wondered how it worked. Here's what I found.

First, MetalLB in layer 2 mode isn't actually a load-balancer. Because of how it works, all of the traffic goes to a
single Kubernetes node. It _does_ manage failover if that node dies.

Here's how traffic gets to the "Welcome to nginx!" nginx instance in my cluster when I browse to `http://nginx.k3s.differentpla.net/`.

## DNS query

There's a DNS query for `nginx.k3s.differentpla.net`. This goes to my router, which forwards it to [a separate CoreDNS
instance]({% post_url 2021/2021-12-29-coredns %}) in the cluster. This looks it up in its hosts file, which is stored in
a ConfigMap.

That has entries for all of the DNS names for exposed services. Most of them go to Traefik at `192.168.28.60`, but nginx
is exposed via a `LoadBalancer` service and is at `192.168.28.61`.

## ARP

To find the host responsible for `192.168.28.61`, the client sends out an ARP request (`who has 192.168.28.61`?). This
is where MetalLB comes in: a "speaker" on the leader node responds with the MAC address of the node. In this case, it's
`48:21:0B:33:03:1E`, which is the node `roger-nuc1`.

So the client opens a TCP connection by sending a SYN packet to that host, with `192.168.28.61` in it. How does that get
to the container?

The MetalLB documentation says "From there `kube-proxy` spreads the traffic to all the service's pods".

## Netfilter

It doesn't do this by (say) listening on port 80 on the node and forwarding the traffic. It does it by messing around
with network filters. You can see this if you run `sudo nft list ruleset`.

```
chain KUBE-SERVICES {
...
  meta l4proto tcp ip daddr 192.168.28.61  tcp dport 80 counter packets 1 bytes 52 jump KUBE-EXT-2CMXP7HKUVJN7L6M
...
```

TCP packets (layer 4) to destination address 192.168.28.61, port 80 are forwarded to the KUBE-EXT-2CMXP7HKUVJN7L6M chain.

That chain is mentioned in a few other rules, such as for the implicit ClusterIP and NodePort services. It looks like this:

```
chain KUBE-EXT-2CMXP7HKUVJN7L6M {
  counter packets 1 bytes 52 jump KUBE-MARK-MASQ
  counter packets 1 bytes 52 jump KUBE-SVC-2CMXP7HKUVJN7L6M
}
```

That does some kind of masquerading thing, and then forwards them to KUBE-SVC-2CMXP7HKUVJN7L6M:

```
chain KUBE-SVC-2CMXP7HKUVJN7L6M {
  meta l4proto tcp ip saddr != 10.42.0.0/16 ip daddr 10.43.80.28  tcp dport 80 counter packets 0 bytes 0 jump KUBE-MARK-MASQ
  counter packets 0 bytes 0 jump KUBE-SEP-T3JGICIP4GFFPRGP
  counter packets 1 bytes 52 jump KUBE-SEP-UPTBB2U3B55FJQPW
  counter packets 0 bytes 0 jump KUBE-SEP-4NAVMHD34SAGR7OJ
}
```

Here's where it does the load-balancing across the services. Those KUBE-SEP-... chains are (I assume) "Service EndPoints". As I do more requests in my browser, I see those counters increase:

```
chain KUBE-SVC-2CMXP7HKUVJN7L6M {
  meta l4proto tcp ip saddr != 10.42.0.0/16 ip daddr 10.43.80.28  tcp dport 80 counter packets 0 bytes 0 jump KUBE-MARK-MASQ
  counter packets 3 bytes 156 jump KUBE-SEP-T3JGICIP4GFFPRGP
  counter packets 3 bytes 156 jump KUBE-SEP-UPTBB2U3B55FJQPW
  counter packets 0 bytes 0 jump KUBE-SEP-4NAVMHD34SAGR7OJ
}
```

Looking at one of those endpoints:

```
chain KUBE-SEP-T3JGICIP4GFFPRGP {
  ip saddr 10.42.0.204  counter packets 0 bytes 0 jump KUBE-MARK-MASQ
  meta l4proto tcp   counter packets 3 bytes 156 dnat to 10.42.0.204:80
}
```

It forwards to 10.42.0.204:80, which is a pod running on a different node -- `roger-nuc0`.

## Flannel

That goes via flannel:

```
roger@roger-nuc1:~ % ip route | grep ^10.42.0
10.42.0.0/24 via 10.42.0.0 dev flannel.1 onlink
```

## More Netfilter?

...and (presumably) magically ends up on the correct node.

I assumed that it went through netfilter again:

```
chain KUBE-ROUTER-FORWARD {
...
  ip daddr 10.42.0.204  counter packets 0 bytes 0 jump KUBE-POD-FW-RQUZ3MEWHD65UI4P
  ip daddr 10.42.0.204 # PHYSDEV match --physdev-is-bridged  counter packets 0 bytes 0 jump KUBE-POD-FW-RQUZ3MEWHD65UI4P
  ip saddr 10.42.0.204  counter packets 0 bytes 0 jump KUBE-POD-FW-RQUZ3MEWHD65UI4P
  ip saddr 10.42.0.204 # PHYSDEV match --physdev-is-bridged  counter packets 0 bytes 0 jump KUBE-POD-FW-RQUZ3MEWHD65UI4P
...
```

```
chain KUBE-POD-FW-RQUZ3MEWHD65UI4P {
  ct state related,established counter packets 0 bytes 0 accept
...
  ip daddr 10.42.0.204  counter packets 0 bytes 0 jump KUBE-NWPLCY-DEFAULT
...
}
```

...but those counters are zero, so maybe not?

## Bridge

It _does_ get routed to the `cni0` interface:

```
roger@roger-nuc0:~ % ip route | grep ^10.42.0
10.42.0.0/24 dev cni0 proto kernel scope link src 10.42.0.1
```

That's a bridge interface which forwards to one of the `veth` devices:

```
roger@roger-nuc0:~ % ip link show
...
8: cni0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1450 qdisc noqueue state UP mode DEFAULT group default qlen 1000
    link/ether 72:88:58:6b:d1:da brd ff:ff:ff:ff:ff:ff
9: veth56cecc8b@if2: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1450 qdisc noqueue master cni0 state UP mode DEFAULT group default
    link/ether f6:00:09:05:bd:fa brd ff:ff:ff:ff:ff:ff link-netns cni-357001b9-24ca-5619-df3a-4dfe972bb930
...more veths
```

...which maps to the default network interface inside the pod. I haven't figured out a reliable way to correlate those
interfaces, so you'll just have to believe me.

And my HTTP request turns up at nginx.
