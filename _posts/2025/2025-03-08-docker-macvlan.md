---
title: Using docker with macvlan on Synology NAS
date: 2025-03-08T12:04:47Z
tags: docker
---

If you use the `bridge` or `host` network drivers in Docker, containers must use different port numbers to be accessible on the host network. Here's how to use the `macvlan` driver to assign a unique IP address to each container.

## Background

By default, when you start a docker container, it uses the `bridge` network driver, which means that only published
ports are accessible, and they're mapped from the host to the container. If instead, you use the `host` network driver,
container ports are host ports.

But this means that the container and the host -- or two containers -- can't use the same port number. This means that
you can't have two webservers both running on port 80 (or port 443). It means that if the host already runs an SSH
daemon on port 22, you can't also access a container's SSH daemon on port 22.

## Motivation

I want to self-host [Forgejo](https://forgejo.org/) on my home network. For various reasons, I _don't_ want to run it in
my K3s cluster. I've got a Synology DS923+, which can run Docker containers, so that seems like a good option. If I use
_Container Manager_, I can set up the Postgres container and the Forgejo container fairly easily. However, because the
NAS is already listening on port 22, I would have to expose Forgejo's SSH daemon on a different port (2222, e.g.).

That seems untidy to me, so I started looking at other options.

## Investigation

It's been a while since I had to do any of this -- Kubernetes just deals with it, but it turns out that the relevant
magic is the `macvlan` network driver. Armed with a relevant search term, I found the following:

- [SynoForum: How to attach container to dedicated interface?](https://www.synoforum.com/threads/how-to-attach-container-to-dedicated-interface.13490/)
- [Creating macvlan in Synology NAS](https://blog.prabir.me/posts/creating-macvlan-in-synology-nas/)
- [Configuring Traefik on Synology DSM7 using docker macvlans](https://blog.ktz.me/configure-traefik-macvlans-synology/) (linked from the previous page)
  - I didn't really make use of this one, but I'm linking to it here, because I suspect I'll want to run Traefik or
    similar as an ingress/reverse proxy at some point.

## Experimenting (not-Synology)

Messing around with network configurations is kinda dangerous if the only way to access the box is _over the network_,
so I did some experimenting on a Linux box I had sitting on my "project" desk. If I screwed it up, it's got a monitor
and keyboard attached, so fixing it would be easier than resetting the NAS.

First, I installed Docker Engine (I'm using Ubuntu) by following the
[instructions](https://docs.docker.com/engine/install/ubuntu/).

Docker's macvlan driver wants an IP address range expressed in CIDR format. When I configured my network, I didn't think
of that, so it's not neatly divided into CIDR-sized chunks. For example, my DHCP server hands out 192.168.28.100 -
192.168.28.254, and my K3s cluster is using 192.168.28.60 - 192.168.28.90. Neither of these line up with CIDR subnets,
so I had to revisit that.

It should be noted that my network isn't _actually_ divided into CIDR-sized subnets -- it's all a single /24 network.
But if docker wants a network specified as a /28 or a /29, it's just easier if I line everything up like that.

I found a handy [Visual Subnet Calculator](https://www.davidc.net/sites/default/subnets/subnets.html) that allowed me to
divide my /24 into CIDR-sized chunks.

I moved the DHCP range to 192.168.28.96 - 192.168.28.254 (which spans a /27 and a /25), and I moved the K3s cluster to
192.168.28.48 - 192.168.28.63 (a /28). Both of these ranges encompass their corresponding old range, so nothing should
break.

In the end, for macvlan, I opted for a /28, giving me 14 IP addresses to play with. That should be plenty.

<div class="callout callout-info" markdown="span">
This would be much easier if I were using IPv6 for everything. I'll revisit that later.
</div>

### Parent network interface

The `macvlan` driver requires a "parent" network interface. You can find this by running `sudo ip link show`. On my Ubuntu PC, this is `eno1`.

### Create macvlan docker network

```sh
sudo docker network create \
    --driver macvlan \
    --subnet 192.168.28.0/24 \
    --gateway 192.168.28.1 \
    --ip-range 192.168.28.64/28 \
    --aux-address 'host=192.168.28.78' \
    --opt parent=eno1 \
    macvlan0
```

This tells docker to create a network, using the `macvlan` driver (`--driver macvlan`).

- The `--subnet` and `--gateway` arguments are those of my home network.
- The `--ip-range` option is the range of addresses reserved for the docker network. In this case it's
  `192.168.28.64/28`. Using a `/28` gives me 16 addresses (14 hosts).
- The `--aux-address` option excludes an IP address from being used in the docker network. The address specified here
  will be used for the host; see below.
- The `--opt parent=eno1` option attaches this network to the `eno1` interface connected to the local network,
  identified earlier.
- The network is named `macvlan0`. We'll use this name later, when starting containers. The name doesn't particularly
  matter, but calling it `macvlan` and adding a number seems like a good choice. I can't see needing more than one or
  two networks.

### Configuring the network

To configure the network interface device, you also need to run the following commands:

```sh
# create the macvlan device; note the 'macvlan0' and 'eno1' from earlier.
sudo ip link add macvlan0 link eno1 type macvlan mode bridge
# give the host an IP address; it's the highest available in the range.
sudo ip addr add 192.168.28.78/32 dev macvlan0
# bring the interface up
sudo ip link set macvlan0 up
# add a route
sudo ip route add 192.168.28.64/28 dev macvlan0
```

These add and configure the `macvlan0` network device. In particular:

- The device is called `macvlan0`, the same as the docker network.
- It's connected to the `eno1` device.
- We give the host the `192.168.28.78` address specified earlier. This is the highest available address in the
  `192.168.28.64/28` CIDR. We could have used the lowest. Doesn't matter as long as they're the same.

### Testing it

To test it, I'm going to start _two_ nginx containers. To tell them apart, I'm going to steal something from [an earlier
blog post]({% post_url 2023/2023-05-01-erlang-k8s-configmap-kustomize %}) and use a volume mount to replace the default
index page.

```sh
mkdir -p tmp/nginx-1 tmp/nginx-2
echo 'One' > tmp/nginx-1/index.html
echo 'Two' > tmp/nginx-2/index.html

sudo docker run --net=macvlan0 --ip=192.168.28.65 --detach --name nginx-1 -v "$(pwd)/tmp/nginx-1:/usr/share/nginx/html" nginx:alpine
sudo docker run --net=macvlan0 --ip=192.168.28.66 --detach --name nginx-2 -v "$(pwd)/tmp/nginx-2:/usr/share/nginx/html" nginx:alpine
```

If I browse to `http://192.168.28.65` or `http://192.168.28.66` from my Windows laptop, I get the expected `One` or
`Two` page. Success.

## Implementing (Synology)

The steps are basically the same as above, but the numbers are different (the Synology NAS is going to get
`192.168.28.32/28`; also `eth0`, rather than `eno1`):

```sh
sudo docker network create \
    --driver macvlan \
    --subnet 192.168.28.0/24 \
    --gateway 192.168.28.1 \
    --ip-range 192.168.28.32/28 \
    --aux-address 'host=192.168.28.46' \
    --opt parent=eth0 macvlan0
```

```sh
sudo ip link add macvlan0 link eth0 type macvlan mode bridge
sudo ip addr add 192.168.28.46/32 dev macvlan0
sudo ip link set macvlan0 up
sudo ip route add 192.168.28.32/28 dev macvlan0
```

### Testing

Basically the same as above; the HTML files go in `/volume1/docker/nginx`.

```sh
mkdir -p /volume1/docker/nginx/nginx-1 /volume1/docker/nginx/nginx-2

echo 'One' > /volume1/docker/nginx/nginx-1/index.html
echo 'Two' > /volume1/docker/nginx/nginx-2/index.html

sudo docker run --net=macvlan0 --ip=192.168.28.33 --detach --name nginx-1 -v "/volume1/docker/nginx/nginx-1:/usr/share/nginx/html" nginx:alpine
sudo docker run --net=macvlan0 --ip=192.168.28.34 --detach --name nginx-2 -v "/volume1/docker/nginx/nginx-2:/usr/share/nginx/html" nginx:alpine
```

And, again, browsing to `http://192.168.28.33` or `http://192.168.28.34`, I see the expected `One` or `Two` responses.

## Conclusions

You can (relatively) easily run containers on a Synology NAS with locally-accessible IP addresses. This means that you
can run multiple containers using the same port number, or where the NAS is also listening on that port.

Whether you _need_ to is another question. Synology Web Station does rudimentary reverse-proxying, including to
containers. It supports associating a different TLS certificate with each service, but the Let's Encrypt integration is
kinda lacking: no wildcards unless you're using Synology's DDNS, and it's very manual.

## What's next?

- I've not made the settings persist over a restart. I'll fix that tomorrow (and update this page). If you're feeling
  impatient, the links above have you covered.
- It would be nice if we didn't have to use IP addresses, so I need to do something with DNS.
  - This will involve some kind of messing around with the router. Currently, adding host entries requires manual steps and restarting things.
  - I solved that already [for the K3s cluster]({% post_url 2021/2021-12-29-coredns %}), so I'm thinking that running CoreDNS inside a container on the NAS would work. I found a [docker plugin](https://github.com/kevinjqiu/coredns-dockerdiscovery) for it. If that doesn't work, I already wrote <https://github.com/rlipscombe/dockerns>.
- TLS and certificates. I need to look at Let's Encrypt.
- Actually installing Forgejo. This is just a prerequisite for SSH access.
