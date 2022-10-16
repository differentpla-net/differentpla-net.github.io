---
title: "Installing ArgoCD Command Line Interface"
short_title: ArgoCD CLI
date: 2022-10-16T09:37:00Z
tags: k3s kubernetes argocd
layout: series
series: k3s
---

ArgoCD provides a web interface and a command line interface. Let's install the CLI.

```
$ wget https://github.com/argoproj/argo-cd/releases/download/v2.4.14/argocd-linux-amd64
$ chmod +x argocd-linux-amd64
$ sudo mv argocd-linux-amd64 /usr/local/bin/argocd
$ argocd login argocd.k3s.differentpla.net
WARNING: server certificate had error: x509: certificate signed by unknown authority. Proceed insecurely (y/n)? y
Username: admin
Password:
'admin:login' logged in successfully
Context 'argocd.k3s.differentpla.net' updated
```

## Creating a new application

Something like this:

```
% argocd app create grafana \
  --repo https://git.k3s.differentpla.net/roger/grafana.git --path . \
  --dest-server https://kubernetes.default.svc --dest-namespace grafana
application 'grafana' created
```
