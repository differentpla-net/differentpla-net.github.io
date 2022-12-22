---
title: "Erlang cluster on Kubernetes: Conclusion"
short_title: "Conclusion"
date: 2022-12-23T18:09:00.000Z
layout: series
series: erlang-cluster-k8s
tags: erlang kubernetes
---

This started as a quick experiment to spin up an Erlang cluster on Kubernetes, using TLS distribution, and to validate
using _cert-manager_ to issue the pod certificates.

It turned into several evenings of writing Erlang code, learning more about Kubernetes and getting annoyed with OpenSSL.

That was followed by the idea to make a YouTube video demonstrating the use of interactive rebase to tidy up commit
history, which is a skill that I feel more developers should know about. That eventually turned into about 8 hours over
several days. I might even get around to releasing the video at some point, though I'm not looking forward to editing it
down from the full 8 hours to something more watchable.

But the whole thing works, so it needed a blog post.

Over the next week or so, that turned into a series of blog posts. There are 21 posts in the series, including the
introduction and this conclusion. It also resulted in the publishing of two other blog posts that are only
tangentially related (so they're not included in the series), plus ideas for at least 4 other posts as yet unwritten.

The series covers the basics of [Erlang clustering][clu], and then [adds TLS][tls]. It demonstrates a bunch of
Kubernetes concepts, such as [init containers][init], [cron jobs][cleanup], headless services, etc.. I spent some time
[reinforcing my hatred][openssl] for OpenSSL, though you only see the end result. There's a bunch of (I hope) good stuff
about using _cert-manager_. Along the way, I spent some time looking at [Erlang's SSH daemon][ssh], and dealt with a
couple of asides, such as running as a [non-root user][non-root] and [using tini as pid 0][pid0].

It's been a _lot_ of work, but I've learnt a load, and I hope it's useful to others.

[clu]: {% post_url 2022/2022-12-21-erlang-cluster-k8s-erlang-clustering %}
[tls]: {% post_url 2022/2022-12-22-erlang-cluster-k8s-tls-distribution %}
[init]: {% post_url 2022/2022-12-22-erlang-cluster-k8s-init-container %}
[cleanup]: {% post_url 2022/2022-12-23-erlang-cluster-k8s-certreq-cleanup %}
[openssl]: {% post_url 2022/2022-12-22-erlang-cluster-k8s-certificate-requests-openssl %}
[ssh]: {% post_url 2022/2022-12-22-erlang-cluster-k8s-ssh %}
[non-root]: {% post_url 2022/2022-12-22-erlang-cluster-k8s-non-root-user %}
[pid0]: {% post_url 2022/2022-12-22-erlang-cluster-k8s-pid-zero %}
