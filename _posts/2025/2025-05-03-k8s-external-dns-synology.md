---
title: "Using Kubernetes External DNS with Synology DNS Server"
date: 2025-05-03T08:49Z
tags: dns synology-srm
---

My router is a Synology RT2600ac; by default it runs `dnsmasq`. I recently replaced it with Synology's full-fat DNS
Service (it's BIND); this broke [DNS for my K3s cluster]({% post_url 2021/2021-12-29-coredns %}); here's how I replaced
that with `ExternalDNS`, making everything work again.

## Motivation

I'm running a number of containers ([Forgejo]({% post_url 2025/2025-03-09-forgejo-synology-1 %}), Immich, etc.) on my
Synology NAS. Because I want to use a wildcard _Let's Encrypt_ certificate for them, I needed them to resolve internally
as (e.g.) `forgejo.differentpla.net`. I initially messed around with editing the dnsmasq configuration files, adding
manual entries to my Pi-Hole server and forwarding the DNS to a [CoreDNS container on the NAS]({% post_url
2025/2025-03-30-coredns-synology %}).

This turned out to be over-complicated and brittle, so I bit the bullet and replaced dnsmasq with BIND, which makes it
easier to add multiple authoritative zones, so I could add `differentpla.net` and `home.differentpla.net`. But when I
attempted to set up forwarding for `k3s.differentpla.net` to the [CoreDNS server for that]({% post_url
2021/2021-12-29-coredns %}), I discovered that BIND (or Synology's UI) doesn't like the fact that it's running on a
non-standard port.

It was always annoying to hand-edit the CoreDNS hosts file, anyway, so this was a good opportunity to see if I could get
[ExternalDNS](https://github.com/kubernetes-sigs/external-dns) working.

Initially, I thought I'd have to write a custom controller to talk to the Synology DNS server API, but then I found this
[Automating DNS with Kubernetes and Synology DSM](https://www.fullstaq.com/knowledge-hub/blogs/automating-dns-with-kubernetes-and-synology-dsm) article. It turns out that the Synology DNS server (because it's BIND) supports Dynamic Updates, following [RFC 2136](https://datatracker.ietf.org/doc/html/rfc2136).

It's a pretty good article, but it left out a couple of details, so I'll write up what I did.

I installed ExternalDNS in my K3s cluster by following the [instructions for the RFC 2136 provider](https://github.com/kubernetes-sigs/external-dns/blob/master/docs/tutorials/rfc2136.md), including RBAC.

## Creating a TSIG key

When you're configuring the deployment, it needs a TSIG key. Here's how you get one:

1. In Synology's "DNS Server" UI, on the left-hand sidebar, click on _Keys_.
2. Click _Create_ / _Create key_.
3. Give it a sensible name. I named it after the DNS zone, but in general, it should be named after the client that's
   going to use it (so you can identify it for revocation later). I only have one client, so it doesn't matter in my
   case.
4. For the algorithm, choose `HMAC-SHA512`.
5. Click _OK_.
6. Click _Export Key_. Your browser will download it.

## Configuring zone updates

You need to configure the zone to accept the key for updates:

1. In Synology's "DNS Server" UI, on the left-hand sidebar, click on _Zones_.
2. In the list, select the relevant zone.
3. Click _Edit_ / _Zone settings_.
4. Make sure that _Limit zone update_ is enabled.
5. Click _Zone Update Rule_.
6. Click _Create_.
7. Select _Key_. In the _Key name_ dropdown, choose the key you just created.
8. Click _Finish_, then _OK_.

## Configuring TSIG in ExternalDNS

The downloaded file will look something like this:

```
key "k3s.differentpla.net" {
algorithm HMAC-SHA512;
secret "RGlkIHlvdSByZWFsbHkgdGhpbmsgSSdkIHB1dCB0aGUgcmVhbCBUU0lHIGtleSBoZXJlPyBIYWhhaGEgLSBOby4=";
};
```

In the deployment YAML for ExternalDNS, edit the container args:

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: external-dns
  namespace: external-dns
spec:
  selector:
    matchLabels:
      app: external-dns
  template:
    metadata:
      labels:
        app: external-dns
    spec:
      serviceAccountName: external-dns
      containers:
      - name: external-dns
        image: registry.k8s.io/external-dns/external-dns:v0.16.1
        args:
        # ...
        - --provider=rfc2136
        # DNS server IP address; this is my router.
        - --rfc2136-host=192.168.28.1
        - --rfc2136-port=53
        # The zone for the updates.
        - --rfc2136-zone=k3s.differentpla.net
        # The 'secret' from above
        - --rfc2136-tsig-secret=RGlkIHlvdSByZWFsbHkgdGhpbmsgSSdkIHB1dCB0aGUgcmVhbCBUU0lHIGtleSBoZXJlPyBIYWhhaGEgLSBOby4=
        # The example has hmac-sha256; we want hmac-sha512; make *sure* you've changed it.
        - --rfc2136-tsig-secret-alg=hmac-sha512
        # The keyname MUST match; it's part of the key.
        - --rfc2136-tsig-keyname=k3s.differentpla.net
        - --rfc2136-tsig-axfr
        - --source=ingress
        - --domain-filter=k3s.differentpla.net
```

Look at the logs for the `external-dns` pod; you should see lines like this:

```
time="2025-05-03T09:36:35Z" level=info msg="Adding RR: erlclu.k3s.differentpla.net 0 A 192.168.28.60"
time="2025-05-03T09:36:35Z" level=info msg="Adding RR: livebook.k3s.differentpla.net 0 A 192.168.28.60"
```

## Troubleshooting

Look at the logs for the pod, _and_ at the logs in Synology DNS console.

- BADKEY - make sure you've specified the key name, algo and secret correctly.
- SERVFAIL - make sure you've got an A record for the nameserver.


## Configuring ExternalDNS sources

The example given in the RFC 2136 tutorial only watches for ingresses, so only ingresses are appearing in the logs
above, and only ingresses are added to the DNS zone.

I've got a couple of `Service` objects with `type: LoadBalancer` and some Traefik `IngressRoute` objects; I wanted these
to appear as well, so I added them to the command args:

```yaml
  # ...
  - --source=ingress
  - --source=service
  - --source=traefik-proxy
  # ...
```

## Configuring RBAC

I also had to change the `ClusterRole`:

```yaml
apiVersion: rbac.authorization.k8s.io/v1
kind: ClusterRole
metadata:
  name: external-dns
  namespace: external-dns
rules:
- apiGroups:
  - ""
  resources:
  - services
  - endpoints
  - pods
  - nodes
  verbs:
  - get
  - watch
  - list
- apiGroups:
  - extensions
  - networking.k8s.io
  resources:
  - ingresses
  verbs:
  - get
  - watch
  - list
# Traefik
- apiGroups:
  - "traefik.containo.us"
  - "traefik.io"
  resources:
  - ingressroutes
  - ingressroutetcps
  - ingressrouteudps
  verbs:
  - get
  - watch
  - list
```

## Adding annotations

With `Ingress` objects, ExternalDNS automatically figures out the DNS host entries. It can't do this for `Service` and
`IngressRoute` objects, so you need to add an annotation to each one. For instance:

```sh
kubectl -n gitea annotate service gitea "external-dns.alpha.kubernetes.io/hostname=git.k3s.differentpla.net"
kubectl -n default annotate service nginx "external-dns.alpha.kubernetes.io/hostname=nginx.k3s.differentpla.net"
```

For `IngressRoute` objects, it looks like you need to annotate the `LoadBalancer` service and the `IngressRoute`:

```sh
kubectl -n kube-system annotate service traefik "external-dns.alpha.kubernetes.io/hostname=traefik.k3s.differentpla.net"
kubectl -n argocd annotate ingressroute argocd-server "external-dns.alpha.kubernetes.io/target=traefik.k3s.differentpla.net"
# I'm not sure whether the next one is required; I added it anyway.
kubectl -n argocd annotate ingressroute argocd-server "external-dns.alpha.kubernetes.io/hostname=argocd.k3s.differentpla.net"
```

I should probably add these to the GitOps repo; I didn't. I'll bite me later, I'm sure.

## Conclusion

It works. I can remove my custom CoreDNS instance from the cluster and the related configuration files from the router,
simplifying my home network configuration.
