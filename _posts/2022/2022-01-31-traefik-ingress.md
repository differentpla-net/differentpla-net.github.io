---
title: "Traefik Ingress"
date: 2022-01-31T08:46:00Z
tags: ingress k3s traefik
---

Rather than use up another LoadBalancer IP address for ArgoCD (and mess around with TLS), let's talk about using an
Ingress. It's entirely possible that I can convert docker and Gitea to use one as well.

There's a really good diagram of how Traefik works [here](https://bryanbende.com/development/2021/05/08/k3s-raspberry-pi-ingress).

When I [installed MetalLB]({% post_url 2021/2021-12-20-installing-metallb %}), I had to
[disable Klipper]({% post_url 2021/2021-12-21-disabling-klipper %}). I assumed that by doing so, I'd completely broken
Traefik. It turns out: no.

```
$ kubectl --namespace kube-system get service traefik
NAME      TYPE           CLUSTER-IP    EXTERNAL-IP     PORT(S)                      AGE
traefik   LoadBalancer   10.43.50.40   192.168.28.10   80:32034/TCP,443:31470/TCP   41d
```

As you can see, Traefik relies on a LoadBalancer service, and once I'd replaced Klipper with MetalLB, it just got an IP
address from MetalLB instead.

If I browse to `http://192.168.28.10/`, it returns a plain text `404 page not found` page, which implies that it's
correctly talking to Traefik.

## Ingress Test

I'm just going to follow [the instructions](https://bryanbende.com/development/2021/05/08/k3s-raspberry-pi-ingress#ingress-test) from the page linked above. I won't bother repeating them here.

```
$ curl http://192.168.28.10/whoami
Hostname: whoami-8557b59f65-6p6pk
IP: 127.0.0.1
IP: ::1
IP: 10.42.3.140
...
```

Yeah; that works.

## Next steps

- Host-based routing. Presumably some [DNS-fettling]({% post_url 2021/2021-12-29-coredns %}) is required.
- TLS termination with Traefik, rather than the individual services.
  - Investigate `cert-manager`.
