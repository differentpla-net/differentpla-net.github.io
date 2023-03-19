---
title: "Expired Certificates: Incident Review"
date: 2023-03-19T20:07:00Z
tags: kubernetes incident-review
---

During the morning of March 18th, 2023, while investigating [this incident]({% post_url
2023/2023-03-18-metallb-incident-review %}), Firefox reported a "your connection is not secure" error when connecting to
my gitea server. This is the incident review.

## Problem

The certificate apparently expired 12 days ago. It does appear to be the correct certificate -- this is important to
note, given that I was investigating a potential problem with DNS or a load-balancer.

## Investigation

I'm using cert-manager for my cluster. Why didn't it renew the certificate automatically?

Cert-manager uses Secrets and Certificates. Let's take a look at those.

```
% kubectl --namespace gitea get secrets
NAME                          TYPE                 DATA   AGE
gitea-postgres-secret         Opaque               1      139d
gitea-tls                     kubernetes.io/tls    2      139d
gitea-inline-config           Opaque               6      139d
gitea                         Opaque               1      139d
gitea-init                    Opaque               2      139d
sh.helm.release.v1.gitea.v1   helm.sh/release.v1   1      139d
sh.helm.release.v1.gitea.v2   helm.sh/release.v1   1      70d
```

```
% kubectl --namespace gitea describe secret gitea-tls
Name:         gitea-tls
Namespace:    gitea
Labels:       <none>
Annotations:  <none>

Type:  kubernetes.io/tls

Data
====
tls.crt:  787 bytes
tls.key:  228 bytes
```

```
% kubectl --namespace gitea get certificates
No resources found in gitea namespace.
```

This TLS secret doesn't appear to be managed by cert-manager.

The certificate expired 2023-03-06, and was issued 2022-02-04, which ... is the date on [this blog post]({% post_url
2022/2022-02-04-gitea-https %}). This means that I'm _not_ using cert-manager for that certificate.

## Immediate actions

- Create a new `Certificate` resource.
- Delete the `gitea-tls` secret.

This will cause cert-manager to take over the responsibility for issuing and renewing the TLS certificate for gitea.

While looking in ArgoCD to see whether -- after deleting the secret.yaml file from git -- it would automatically delete
the secret as well, I discovered that the certificate for ArgoCD had _also_ expired.

I also realised that this wouldn't work anyway -- if the gitea certificate had expired, ArgoCD wouldn't be able to
refresh the application.

So I deleted the secret and applied the `certificate.yaml` file, both manually. Once I restarted the `gitea-0` pod, this
resolved the problem.

Gitea uses a Certificate resource, rather than an Ingress, because it has an SSH server, and needs its own
load-balancer.

Similarly, ArgoCD uses an IngressRoute, rather than an Ingress -- I don't recall why, but it needed the same fix: delete the existing Secret resource and create a new Certificate resource.

## Future Actions

- Audit the cluster to see if there are any other TLS secrets that aren't using cert-manager.
- Investigate why gitea didn't load the new certificate and whether that'll be a problem when it auto-renews.
