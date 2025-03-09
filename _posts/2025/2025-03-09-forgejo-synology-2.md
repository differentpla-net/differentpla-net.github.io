---
title: Installing Forgejo on Synology NAS, part 2
date: 2025-03-09T14:32:47Z
tags: forgejo synology-nas
---

In [part 1]({% post_url 2025/2025-03-09-forgejo-synology-1 %}), I'd got Forgejo running on my Synology NAS, but SSH
access wasn't working properly. In this part, I'm going to fix that by using HAProxy.

## The problem

In order to terminate TLS, I put an socat in front of Forgejo. This is on `192.168.28.32`, and
`forgejo.differentpla.net` points to it. The Forgejo container (and sshd) is running on `192.168.28.33`.

This means that `git clone git@forgejo.differentpla.net:user/repo` fails with `Connection refused`, because it's not
listening for SSH connections on that host.

## HAProxy

Because socat can't forward more than one port at a time, I'm going to have to replace it with something more
full-featured. I'm going to use [HAProxy](https://www.haproxy.org/) because I haven't used it in a few years, so why
not?

## `compose.yaml`

I replaced the relevant section of the compose file with the following:

```yaml
  proxy:
    image: haproxy:3.1.5-alpine
    user: root
    volumes:
      - /volume1/docker/forgejo/haproxy.cfg:/usr/local/etc/haproxy/haproxy.cfg
      - /volume1/docker/forgejo/certs:/certs:r
    networks:
      macvlan0:
       ipv4_address: 192.168.28.32
      forgejo:
```

Of note:

- `user: root` is required, otherwise you get `cannot bind socket` errors.
- I've volume-mounted the `haproxy.cfg` file.
- The certificates are mounted as before.

## Certificates

HAProxy wants the key and certificate in the same file:

```sh
cd /volume1/docker/forgejo/certs
cat forgejo-differentpla-net.crt forgejo-differentpla-net.key > forgejo-differentpla-net.pem
```

Note that the CA certificate doesn't need to be in the PEM file. So we don't include it.

## `haproxy.cfg`

The `haproxy.cfg` file looks like the following:

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
    bind *:443 ssl crt /certs/forgejo-differentpla-net.pem
    mode http
    http-request redirect scheme https unless { ssl_fc }
    default_backend forgejo_http

backend forgejo_http
   mode http
   server forgejo forgejo:3000

frontend ssh
   bind *:22
   mode tcp
   default_backend forgejo_sshd

backend forgejo_sshd
   mode tcp
   server forgejo forgejo:22
```

## Testing it

Browsing to `http://forgejo.differentpla.net` redirects to `https://forgejo.differentpla.net`, so that's all good.

After creating a user, adding SSH public keys, and creating a test repository, `git clone
git@forgejo.differentpla.net:roger/test.git` also works.

## Conclusion

It works.

### What's missing?

- Backup and restore.
- Certificates from Let's Encrypt. That's not strictly a Forgejo concern though, so I'll deal with that later.
  - I've got to change my DNS hosting before I can really deal with this. I'll probably change registrar at the same time.
- Monitoring.
