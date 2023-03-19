---
title: "MetalLB ARP failure: Incident Review"
date: 2023-03-18T11:54:00Z
tags: kubernetes incident-review
---

On the morning of March 18th, 2023, accessing any sites hosted by my Kubernetes cluster would fail with a "Connection
Timed Out" error. This is the incident review.

## Background

I have a K3s cluster of 4 nodes, using Intel NUCs. On Friday afternoon, I was moving the nodes from one side of the
office to the other when one of the nodes failed to power on. This left all of my longhorn volumes in a degraded state,
but -- hey -- it was Friday night, this is only my homelab, so I decided to ignore it until Saturday morning.

## Problem

On Saturday morning (March 18th), I attempted to view the Longhorn dashboard to see if the degraded volumes had been
rebalanced across the three surviving nodes. This failed with a "Connection Timed Out" error in Firefox.

A brief investigation revealed that I couldn't get to any of the sites hosted via Traefik on the cluster.

I use a [separate CoreDNS instance]({% post_url 2022/2022-02-25-coredns-custom %}) for sites running on the cluster,
[MetalLB]({% post_url 2021/2021-12-20-installing-metallb %}) as a bare-metal load-balancer, and Traefik as an ingress
controller.

Some services don't use Traefik and have their own load-balancer addresses. These are either services where an Ingress
won't work (e.g. [gitea]({% post_url 2022/2022-01-30-gitea %})), or where I hadn't figured out how to use an Ingress yet
(e.g. a do-nothing installation of nginx).

## Investigation: DNS?

It's _always_ DNS. Was it DNS in this case?

All queries for ingress-based services returned the correct .60 address, which is the load balancer attached to Traefik,
so it didn't look like a problem with DNS.

## Investigation: MetalLB?

This meant that it could be a problem with the load-balancer (MetalLB), or it could be a problem with the ingress
controller (Traefik).

To remind me of which services use Traefik, and which use their own load-balancer, I looked at the CoreDNS
configuration with `kubectl --namespace k3s-dns get cm k3s-dns -o yaml`.

Looking at two of the sites with their own load-balancers, one (nginx) timed out, and the other (gitea) gave me a security
warning.

Because nginx was also broken -- and doesn't use Traefik -- I suspected the problem was with MetalLB. The security
warning from gitea will be addressed in another incident report.

To rule out problems with Firefox, I tried using curl directly and got "no route to host"; other hosts on the same
subnet work fine. This implies a problem with MetalLB: I use it in ARP mode, and if it's not responding to ARP requests,
this is the usual failure mode.

Running `kubectl --namespace metallb-system get pods -o wide` revealed 1 controller pod and 4 "speaker" pods, including
one apparently still running on the dead node.

Explicitly deleting that pod didn't immediately resolve the problem. Looking in the logs for one of the other pods
showed that it was still attempting to connect to the dead pod. So I simply deleted all of the "speaker" pods.

After a short while, running `curl http://x.y.z.60` returned "404 page not found", which is from Traefik, rather than
"no route to host", so it looked like the problem was resolved.

In Firefox, navigating to the Longhorn dashboard was working again, so I called the incident resolved.

## Immediate actions

- I restarted all of the metallb speaker pods, to fix ARP.

## Future investigation

- Why didn't K8s delete the pod from the dead node?
- Why didn't manually deleting it resolve the problem?

## What went well?

Once I sat down to investigate, the problem was solved in about 35 minutes, which is fine for a homelab.

## What went badly?

Not a lot.
