---
title: Chef Server, Chef Zero or Chef Solo?
date: 2014-11-13 11:06:34
tags: chef
---

*This is part of my [quest to sort out my website]({% post_url 2014-11-13-website-rebuild %}).
Which Chef?*

Chef is available in several "editions", and I struggled to choose one. Here's
my understanding:

## Chef Server

The preferred way to run Chef in a large server farm is with each server
("node" in chef parlance) talking to a centrally-managed Chef server.

You can use Opscode's hosted chef server, or you can use an on-premises chef
server.  See [https://manage.opscode.com/](https://manage.opscode.com/) for
more details.

## Chef Zero

Per [https://github.com/opscode/chef-zero](https://github.com/opscode/chef-zero):

> Chef Zero is a simple, easy-install, in-memory Chef server that can be useful
> for Chef Client testing and chef-solo-like tasks that require a full Chef
> Server.

## Chef Solo

Per [https://docs.getchef.com/chef_solo.html](https://docs.getchef.com/chef_solo.html):

> chef-solo is an open source version of the chef-client that allows using
> cookbooks with nodes without requiring access to a Chef server. chef-solo
> runs locally and requires that a cookbook (and any of its dependencies) be on
> the same physical disk as the node.

## So, which one?

 - For managing a single server, chef server seemed like overkill.
 - Chef zero is great, but I didn't like the way that the node configuration
   (roles, attributes, etc.) are kept in the server, and can't easily be
   versioned with the other files.
 - So I opted for Chef Solo, of which more later.
