---
title: "Finding it difficult to export content from Drupal"
date: 2011-12-20T16:14:38.000Z
---
This is some background to my [Finding it difficult to export content from #drupal.](https://twitter.com/#!/rogerlipscombe/status/146682447667544064) tweet.

I've been using drupal for about a million years, and generally like it. However, it doesn't really have first-class support for embedded images, and I always run into difficulty when upgrading it.

So, because I'm a developer, my first thought was to write my own blog engine. This is (or was, 5 years ago) the modern equivalent of a "Hello World" program. It was going to have XML-RPC support, so that you could [use Live Writer](http://www.labs.jobserve.com/Articles.aspx/Building-Labs--XML-RPC-Backend-for-Windows-Live-Writer-in-C-with-WCF).

First, though I needed to export my content from drupal, probably to BlogML. This is harder than it should be. Hence the tweet.

I made a start, but I don't have as much time to work on personal projects as I used to, so I've given up. It turns out that [writing a blog engine is harder than you'd think](http://haacked.com/archive/2006/10/06/Rolling_Your_Own_Blog_Engine.aspx).

That said, there's some code at [https://bitbucket.org/rlipscombe/tangofox](https://bitbucket.org/rlipscombe/tangofox) that implements a simple XML-RPC backend, based on [Clemens Vaster's implementation](http://vasters.com/clemensv/PermaLink,guid,679ca50b-c907-4831-81c4-369ef7b85839.aspx), and also has the beginnings of a drupal-to-BlogML exporter, using [BlogML .NET](http://blogml.codeplex.com/).
