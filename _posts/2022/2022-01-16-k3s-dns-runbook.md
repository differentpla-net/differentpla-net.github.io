---
title: "Custom CoreDNS Runbook"
date: 2022-01-16T10:08:00Z
tags: runbook
layout: series
series: k3s
---

I recently left my k3s cluster turned off for a week or so. When I turned it back on, the [`k3s.differentpla.net` DNS]({% post_url 2021/2021-12-29-coredns %}) wasn't working. Let's figure it out and maybe write a runbook for the next time.

## Problem: Server Not Found

Browsing to `https://nginx.k3s.differentpla.net/` from my Windows laptop fails with "Server Not Found", rather than the expected nginx placeholder.

## Approach

There are a number of things that could be broken:

1. Client DNS resolution on Windows.
2. DNS server on CoreDNS.
3. Load Balancer.
4. nginx not running?
5. Something else?

Let's start ruling these out.

## Investigation: DNS

It's always DNS.

If I try `nslookup nginx.k3s.differentpla.net` from Windows, the query times out. This suggests that it's a DNS problem.
You'll recall that the DNS query initially goes to the router, which forwards it to a `NodePort` service on the cluster.

Let's see if directly querying CoreDNS on the cluster works:

```
$ dig +short -p 32053 @rpi401 nginx.k3s.differentpla.net
192.168.28.11
```

It does. Our problem is therefore (probably) somewhere between the router and the cluster. If it hadn't worked, we would
have needed to dig into pod status, etc.

Annoyingly, Synology SRM uses Busybox; it doesn't include `dig`, and the `nslookup` implementation doesn't allow
specifying an alternative port number, so we're kinda stuck.

The DNS forwarding is controlled by the `/etc/dhcpd/dhcpd-k3s-dns.conf` file, which looks like this:

```
server=/k3s.differentpla.net/192.168.28.181#32053
```

So it's forwarding the query to `192.168.28.181:32053`. Is that the correct IP address?

```
SynologyRouter> nslookup rpi401 localhost
Server:    127.0.0.1
Address 1: 127.0.0.1 localhost

Name:      rpi401
Address 1: 192.168.28.180
```

No; it's not. Seems the IP address of `rpi401` changed. Presumably because the DHCP lease expired while the cluster was turned off.

## Potential Fix: Use hostname when forwarding DNS?

I considered changing the forwarding rule to use the name, rather than the IP address, like this:

```
# This won't work.
server=/k3s.differentpla.net/192.168.28.181#32053
```

But it doesn't work:

```
SynologyRouter> nslookup rpi401
nslookup: can't resolve 'rpi401'
```

## Fix: DHCP reservation

So it looks like we'll have to set up a DHCP reservation for (at least) the control plane node (rpi401) of our cluster. That's a relatively simple fix in the Synology SRM user interface.

1. Log into SRM.
2. Open "Network Center".
3. Go to the "Local Network" section.
4. In the "DHCP Clients" tab, verify that nothing's using the `.181` IP address.
   - This step is to avoid editing the DNS forwarding on the router, because that requires announcing a maintenance window (for restarting dnsmasq on the router) to the family. Fortunately, this address wasn't in use.
5. Grab the MAC/DUID for `rpi401`.
6. In the "DHCP Reservation" tab, add a reservation. You'll need the MAC/DUID from the previous step (or you can grab it from the node with `ip addr show dev eth0`).
7. **This doesn't work**: Renew the node's IP address with `sudo networkctl renew eth0`.
8. Restart networking on the pod with `systemctl restart systemd-networkd`. This will kill your SSH session.

With any luck, the node will now have the reserved (`.181`) IP address.

## Problem: DNS still broken

The node now has the correct IP address, but the `dig` command from above is now broken:

```
$ dig +short -p 32053 @rpi401 nginx.k3s.differentpla.net
...hangs...
```

According to `kubectl --namespace k3s-dns get all`, the pod is running and the service is correct. Everything should be
fine. For now, I'm going to assume that the changing IP address confused CoreDNS. Maybe deleting and recreating the pod
will fix it?

It didn't.

According to `kubectl --namespace k3s-dns get pods -o wide`, the pod is running on `rpi404`. Does _that_ work?

```
$ dig +short -p 32053 @rpi404 nginx.k3s.differentpla.net
192.168.29.11
```

Yes; it does. Obviously the NodePort service is messed up. Let's try deleting and recreating it:

```
$ kubectl --namespace k3s-dns delete service k3s-dns
service "k3s-dns" deleted

$ kubectl apply -f k3s-dns/svc.yaml
service/k3s-dns created

$ kubectl --namespace k3s-dns get service
NAME      TYPE       CLUSTER-IP      EXTERNAL-IP   PORT(S)                     AGE
k3s-dns   NodePort   10.43.108.230   <none>        53:32053/TCP,53:32053/UDP   3s
```

Nope:

```
$ dig +short -p 32053 @rpi401 nginx.k3s.differentpla.net
...hangs...
```

Yeah; it's still broken. If this were a production cluster, I'd be a bit less gung-ho, but it's not, so I'm gonna just
restart the `rpi401` node. Of course, if this were a production cluster, I'd have production-grade DNS, and this wouldn't be necessary.

It takes about 90 seconds for the node to restart, but that seems to fix it.

## Lessons

1. If you're relying on an IP address not changing, make sure it doesn't change. Consider fixed (reserved) IP addresses.
2. Kubernetes doesn't seem to care if you restart nodes. It just keeps chugging.
3. It does care if you reconfigure the network under its feet.
4. You're still going to need monitoring.
