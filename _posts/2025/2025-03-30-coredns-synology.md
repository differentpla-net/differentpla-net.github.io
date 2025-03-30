---
title: "Running a CoreDNS container on a Synology NAS"
date: 2025-03-30T17:38Z
tags: core-dns synology-nas
---

My home DNS is a bit of a mess, so I want to install CoreDNS on my Synology NAS to tidy it all up. I'll run it in a
container.

## Background

As I add more containers to my local network (Forgejo, Homepage, etc.), I need to add them to DNS.

My router (Synology SRM) runs dnsmasq, so this involves logging into the router with SSH and adding individual entries
to either the dnsmasq configuration files or `/etc/hosts`. This is very manual and very brittle.

An alternative would be to install Synology's full-fat DNS service (it's a copy of BIND) on the NAS, and have the router
forward local queries to that. That would certainly work, but BIND seems like overkill, and I don't like needing to use
the Synology web interface to add new entries.

## CoreDNS

So, since I've got experience with running a custom CoreDNS server in my K3s cluster, I'm going to install CoreDNS in a
container and use that.

## Creating a container project

On the Synology NAS: _Container Manager_ / _Project_ / _Create_

- Project name: coredns
- Path: /docker/coredns
- Source: Create docker-compose.yml

The `docker-compose.yaml` file looks like this:

```yaml
services:
  coredns:
    image: coredns/coredns:1.12.0
    command: -conf /root/Corefile
    restart: always
    volumes:
      - /volume1/docker/coredns:/root:ro
    networks:
      macvlan0:
        ipv4_address: 192.168.28.45

networks:
  macvlan0:
    name: macvlan0
    external: true
```

I (fairly arbitrarily) gave it the second-highest IP address in the range. It feels like network things go at the top,
server things at the bottom.

- No web portal.

## Corefile

The `Corefile` looks like this:

```
. {
  reload
  errors
  log
}

differentpla.net {
  hosts {
    192.168.28.32 forgejo.differentpla.net
    192.168.28.33 homepage.differentpla.net
    192.168.28.45 coredns.differentpla.net
  }
}
```

At this point, I was too lazy to put together a proper DNS zone file, so I manually added the entries. By using the
`reload` plugin, I only need to edit the file; I don't need to manually restart anything.

I'd prefer to use a docker discovery plugin, because then I could use docker labels both for DNS discovery and for
Caddy/Traefik routing (when I get around to installing one of them), but there doesn't seem to be an officially-listed
one. That's a job for another day.

## Testing it

```
% dig +short @192.168.28.45 homepage.differentpla.net
192.168.28.33
```

That seems to work.

## Configuring the router

```
SynologyRouter:/etc/dhcpd # cat dhcpd-differentpla-net.conf
server=/differentpla.net/192.168.28.45#53
local=/internal.differentpla.net/

SynologyRouter:/etc/dhcpd # cat dhcpd-differentpla-net.info
enable="yes"

SynologyRouter:/etc/dhcpd # /etc/rc.network nat-restart-dhcp
```

Note: you MUST have two hyphens in the name.

## Configuring Pi-hole

Previously, I had some custom CNAME and A records configured in my Pi-hole instance (under "Local DNS"). I removed
those.

I also changed the DNS settings ("Settings" / "DNS") to remove the Google servers and add my router as "Custom 1".

## Conclusion

Now, when I look up (e.g.) `forgejo.differentpla.net` from inside my local network, the query first goes to Pi-hole,
which forwards it to my router, which forwards it to the CoreDNS container, which answers with the correct IP address.

Yes, it's a bit complicated. Kinda limited by using Synology SRM, rather than pfSense, OPNsense or OpenWRT or similar.
