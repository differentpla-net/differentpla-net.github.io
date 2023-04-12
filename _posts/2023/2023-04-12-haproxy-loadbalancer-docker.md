---
title: "Using HAProxy as a load balancer with docker"
date: 2023-04-12T13:27:00.000Z
tags: docker haproxy
---

If you're running multiple copies of the same container in docker, and you don't care which one you connect to, you
might want to use HAProxy as a layer 4 (TCP) load balancer.

If your containers are using HTTP, then you can use any reverse proxy, such as nginx, or caddy, or HAProxy as a layer 7
proxy.
