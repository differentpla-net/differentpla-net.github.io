---
title: Installing Forgejo on Synology NAS, part 1
date: 2025-03-09T10:06:47Z
tags: forgejo synology-nas
---

I want to install Forgejo on my Synology NAS. I'm going to attach it to the macvlan network I createed yesterday. I'm
not going to use Synology's built-in Web Portal (reverse proxy thing). Here's how I did it.

## File Station

You'll need to set up storage for both the Forgejo server and the database server (I'm using PostgreSQL). To do that:

1. In _File Station_, navigate to the `docker` directory.
2. Inside the `docker` directory, create a `forgejo` sub-directory.
3. Inside the `forgejo` directory, create `db` and `data` directories.

Alternatively, run the following commands:

```sh
mkdir -p /volume1/docker/forgejo/data
mkdir -p /volume1/docker/forgejo/db
```

## Container Manager

1. In _Container Manager_, select _Project_ from the sidebar.
2. Click the _Create_ button.
3. Enter `forgejo` as the project name.
4. Set the path to the directory we created above, `/docker/forgejo`.
5. For the source, select "Create docker-compose.yml" and paste in the Compose file below.
6. _Don't_ set up the web portal via Web Station.

### Compose file

```yaml
services:
  forgejo:
    image: codeberg.org/forgejo/forgejo:9
    environment:
      - FORGEJO__database__DB_TYPE=postgres
      - FORGEJO__database__HOST=postgres:5432
      - FORGEJO__database__NAME=forgejo
      - FORGEJO__database__USER=forgejo
      - FORGEJO__database__PASSWD=forgejo
    restart: always
    networks:
      - forgejo
      - macvlan0
    volumes:
      - /volume1/docker/forgejo/data:/data:rw
      - /etc/TZ:/etc/timezone:ro
      - /etc/localtime:/etc/localtime:ro
    depends_on:
      - postgres

  postgres:
    image: postgres:17
    restart: always
    healthcheck:
      test: ["CMD", "pg_isready", "-q", "-d", "forgejo", "-U", "forgejo"]
      timeout: 45s
      interval: 10s
      retries: 10
    environment:
      - POSTGRES_USER=forgejo
      - POSTGRES_PASSWORD=forgejo
      - POSTGRES_DB=forgejo
    networks:
      - forgejo
    volumes:
      - /volume1/docker/forgejo/db:/var/lib/postgresql/data:rw

networks:
  forgejo:
    external: false
  macvlan0:
    name: macvlan0
    external: true
```

Note that a `compose.yaml` will be written to the directory specified.

### On (not) using Web Station / Web Portal

You _could_ use Web Station for fronting Forgejo's HTTP server, but I chose not to. Here's why:

- I'm going to give the container its own IP address (by using the macvlan driver), so I don't need to.
- Web Portal registers the `forgejo.local` name using mDNS. I don't like mDNS.
- I want to access the container from my K3s cluster, which means that mDNS won't work.
- I'll probably want to access the container from Tailscale, which also rules out using mDNS.
- I want to enable HTTPS, which means I need a certificate.
  - I don't want to use a private CA, which means that `forgejo.local` won't work anyway.
- Certificate management in Synology works...
  - You add your certificate in _Control Panel_ / _Security_ / _Certificate_.
  - Then, in the same page, you click _Settings_ and associate your service with that certificate.
  - ...but it's all very manual, and I want to use Let's Encrypt, which (because of the short expiries) kinda requires
    automation.

Note that if you want to use the Web Portal later, it's annoying (or impossible; I couldn't find the settings) to add
the relevant settings later.

This is another reason why I chose not to use Web Station: it seems like it was originally built for PHP applications,
and the container support feels like a poorly-integrated afterthought.

If I need a reverse proxy (and I probably will, later), I'll look at nginx, or Caddy, or even Traefik.

## Port 80

Setting `FORGEJO__server__HTTP_PORT=80` results in a bind error. After some searching, I found
[forgejo#4171](https://codeberg.org/forgejo/forgejo/issues/4171) which explains that Forgejo runs as an unprivileged
user.

A later comment in that issue, however, shows how to run nginx as a reverse proxy on port 80, forwarding to port 3000.

Since I'm a complete cowboy, however, I'm just going to run socat as a port forwarder instead. That looks like this:

```yaml
services:
  # ...
  proxy:
    image: alpine/socat:latest
    command: "TCP-LISTEN:80,fork,reuseaddr TCP:forgejo:3000"
  networks:
    macvlan0:
      ipv4_address: 192.168.28.32
    forgejo:
```

Note the `ipv4_address` line. This gives the container a fixed IP address on the `macvlan0` network, meaning that we can
point DNS at it later without it changing.

Don't remove the `macvlan0` network from the `forgejo` service, otherwise SSH access breaks.

I'll replace this with an actual reverse proxy later.

## DNS

I'm running Pi-hole, so this is relatively simple:

1. Open the Pi-hole administration pages.
2. Go to _Local DNS_ / _DNS Records_.
3. Add an entry for `forgejo.differentpla.net` (or whatever) and point it to `192.168.28.32`.

I also wanted to create a `git.differentpla.net` alias, mostly for SSH access:

1. Go to _Local DNS_ / _CNAME Records_.
2. Add an entry for `git.differentpla.net` and point it to the name you chose earlier (in my case
   `forgejo.differentpla.net`).

## TLS

I'm not in a position to use Let's Encrypt for TLS certificates yet, but I can at least use a self-signed certificate or
private CA for now.

I'll use my [elixir-certs script]({% post_url 2021/2021-12-21-elixir-certs %}):

```sh
./certs create-cert \
    --issuer-cert differentpla-net-root.crt \
    --issuer-key differentpla-net-root.key \
    --out-cert forgejo-differentpla-net.crt \
    --out-key forgejo-differentpla-net.key \
    --template server \
    --subject '/CN=forgejo.differentpla.net'
```

Then I created the `/volume1/docker/forgejo/certs` directory and copied the keypair to it.

**No, this is not good PKI practice.** I did `chmod 600` the two files, though.

To get `socat` to use this, I changed the entry in the compose file to the following:

```yaml
services:
  # ...
  proxy:
    image: alpine/socat:latest
    command: "OPENSSL-LISTEN:443,cafile=/certs/differentpla-net-root.crt,key=/certs/forgejo-differentpla-net.key,cert=/certs/forgejo-differentpla-net.crt,verify=0,fork,reuseaddr TCP:forgejo:3000"
    volumes:
      - /volume1/docker/forgejo/certs:/certs:r
    networks:
      macvlan0:
       ipv4_address: 192.168.28.32
      forgejo:
```

Specifically:

- The command changes to `OPENSSL-LISTEN:443` with the `cafile`, `key` and `cert` options. Importantly, socat defaults
  to asking for client certificates. We don't want that, so we use `verify=0`.
- Mount the directory containing the certificates at `/certs`.

## Configuration

At this point, I can browse to `http://forgejo.differentpla.net`, and I'm presented with the Forgejo installation page.

The default settings are mostly fine. I changed the instance title and slogan. I disabled OpenID authentication.

The _Server domain_ setting (used for SSH) should be set to `forgejo.differentpla.net` and the _Base URL_ setting should
be `https://forgejo.differentpla.net`.

You need to set up an administrator account. It's slightly annoying that you can't use `admin` as the Administrator
username, because it's reserved, however. I'm sure you can come up with something.

Click the _Install Forgejo_ button and wait for about a minute.

## Conclusion

I've now got a self-hosted Forgejo instance running on my Synology NAS.

What _doesn't_ work is SSH access, which was the point of this weekend's exercise. This is because
`forgejo.differentpla.net` resolves to the `socat` container (listening on port 443, not port 22), rather than the
`forgejo` container (where sshd is listening on port 22).

To fix this, I'm going to need something a bit more full-featured than socat. More on this in a later blog post.
