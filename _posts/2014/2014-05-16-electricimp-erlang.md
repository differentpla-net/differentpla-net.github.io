---
title: Electric Imp Moves to Erlang Programming Language
date: 2014-05-16T00:00:00Z
tags: erlang electric-imp
---

*Originally posted to the Electric Imp blog. Preserved here.*

At the heart of the [Electric Imp](https://electricimp.com) platform is our cloud connectivity service which provides
the secure connection between every imp out in the wild and their corresponding agents. From there, imps can interact
with the world - smartphone apps, web services, databases and so on.

This critical piece of infrastructure deals with connection security, ensuring every imp has the latest code to run, and
that the connection load is distributed evenly across our server farms. Until now, this service has been implemented in
node.js, a high performance javascript based platform, but today we’re announcing that this piece is moving to Erlang.

What is Erlang? It’s a programming language designed from the ground up to build massively scalable systems with high
availability, and is extensively used in telecoms, banking and e-commerce applications. Just as those markets are very
demanding of their platforms, so are we.

Erlang is the right foundation for our platform because we get a system that enables us to focus on adding features,
while the language, libraries and runtime take care of scalability, distribution and fault tolerance.  Improved
scalability means that we can smoothly handle the increasing number of devices connected to the Electric Imp platform.
Distribution and fault tolerance mean that if something does go wrong you should not notice.

We’re particularly excited that Erlang supports zero-downtime upgrades. We are still experimenting with this feature in
depth, but it does mean - for most things - that in the future we’ll no longer need maintenance windows. We’ll be able
to release new features and fixes with no disruption to the service we offer.

## Where Are We Now

We’ve been running the Erlang connectivity service on our staging environment for the last couple of months, and beta
testers have been using an Erlang production server for the past few weeks. We plan to start moving developer imps to
the Erlang servers this week, and if all goes smoothly the production servers will switch to Erlang over the next month
or so.

Generally, the move should be completely transparent to both developers and end users, and we have not made any changes
to the server components that host your agents and handle your HTTP requests. If any issues do arise that we have not
anticipated during our extensive internal testing, we will be investigating and addressing each one as they emerge.

## What’s Next?

The Internet of Things promises to change the way we connect and interact with devices and each other in a profound
ways. The transition to Erlang is just one of the many ways Electric Imp will deliver on that promise and continue to
offer the most advanced, scalable and innovative connectivity platform possible.

-- Roger Lipscombe, Backend Developer

<div class="callout callout-info" markdown="span">
This post is no longer available on the Electric Imp blog, so to preserve it, I took a copy from the [Internet Archive Wayback Machine](https://web.archive.org/web/20140702010121/http://blog.electricimp.com/post/85932940180/electric-imp-moves-to-erlang-programming-language).
</div>
