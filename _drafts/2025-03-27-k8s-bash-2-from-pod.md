---
title: "Kubernetes API from bash: from a pod"
date: 2025-03-27T17:28Z
short_title: "From a pod"
tags: kubernetes curl bash
layout: series
series: k8s-bash
---

Here's how to access the Kubernetes API from bash, in a pod, using curl.

kubectl run --stdin --tty ubuntu-curl2 --image=ubuntu-curl2 --image-pull-policy=Never

Inside a pod, the Kubernetes API server is identified

See also _posts/2022/2022-01-07-k8s-api-in-container.md