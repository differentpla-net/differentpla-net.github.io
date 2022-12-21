---
title: "Erlang cluster on Kubernetes: Introduction"
short_title: "Introduction"
date: 2022-12-21T19:40:00.000Z
layout: series
series: erlang-cluster-k8s
tags: erlang kubernetes
---

A few weeks ago, I decided to write a blog post about using mutual TLS to secure Erlang distribution (clustering), with
auto-provisioning of certificates when running in Kubernetes. It took a little longer to write up than I expected, and
turned into a series of blog posts.

{% include _series_toc.html %}

You can follow along by cloning the [rlipscombe/erlang-cluster](https://github.com/rlipscombe/erlang-cluster) project
from Github. The posts in this series and the merge commits in that project are broadly lined up.

I also spent about 8 hours rewriting the commit history of the project so that it forms a coherent story. Please feel
free to read it as one.

## Related posts

- [Erlang clustering recap]({% post_url 2022/2022-11-11-erlang-clustering-recap %})
- [Erlang TLS Distribution]({% post_url 2022/2022-11-12-erlang-tls-distribution %})
- [Options for automatically creating certificates for mutual pod authentication]({% post_url 2022/2022-11-13-cert-signing-options %})
- [Installing cert-manager]({% post_url 2022/2022-02-06-cert-manager %})
