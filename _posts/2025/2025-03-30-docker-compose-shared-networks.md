---
title: "Sharing networks between multiple docker compose projects"
date: 2025-03-30T11:20Z
tags: docker
---

I run a number of docker containers on my Synology NAS, and I want to is put a reverse proxy in front of them. Since
they're defined in separate docker compose projects, I need to attach the reverse proxy to the network defined by each
project.

Here's a simple example. I'll put a reverse proxy in one project and nginx in another project, and connect the two.

## Reverse proxy

For the reverse proxy, I'll use HAProxy. The first part of the compose file is this:

```yaml
# proxy.compose.yaml
services:
  proxy:
    image: haproxy:3.1.5-alpine
    user: root
    volumes:
      - ./haproxy.cfg:/usr/local/etc/haproxy/haproxy.cfg
```

We use the `alpine` image, because it's smaller; we run as `root` (for port 80); the `haproxy.cfg` (see below) file is
mounted as a volume.

The `proxy` service is then attached to two separate networks:

```yaml
    # ...continued from above.
    networks:
      macvlan0:
        ipv4_address: 192.168.28.32
      nginx:
```

The first network, `macvlan0` is the one I defined in [this post]({% post_url 2025/2025-03-08-docker-macvlan %}). The
second is the network where the `nginx` container will be running.

The two networks are defined at the top level of the compose file, as follows:

```yaml
networks:
  macvlan0:
    name: macvlan0
    external: true
  nginx:
    name: nginx
    external: true
```

Both networks are defined as `external: true`, because they're not defined in this compose file.

The haproxy.cfg file looks like this:

```
global
    log stdout format raw local0 info

defaults
    log global
    maxconn 20000
    timeout connect 5000ms
    timeout client  50000ms
    timeout server  50000ms

frontend http
    bind *:80
    mode http
    default_backend nginx

backend nginx
   mode http
   server default nginx.nginx:80
```

The interesting bit in here is the `server default nginx.nginx:80`, which uses the backend server `nginx.nginx`. This is
the service name (`nginx`) and the network name (`nginx`) defined previously.

## Web server

For the web server, I'll use nginx. The compose file for that looks like this:

```yaml
# nginx.compose.yaml
services:
  nginx:
    image: nginx:alpine
    networks:
      - nginx

networks:
  nginx:
    name: nginx
    external: false
```

It defines the `nginx` service and attaches it to the `nginx` network. The network is defined at the top level of the
compose file. This time we use `external: false`, because the network _is_ defined in this compose file. The name used
in the other compose file matches the name defined here.
