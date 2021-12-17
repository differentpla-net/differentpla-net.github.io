---
title: No new blog engine
date: 2015-04-26 08:13:00Z
tags: blogging
---

Periodically, I become unhappy with my blog implementation and decide that I
should do something about it.

Then, because [writing a blog engine is harder than you
think](http://haacked.com/archive/2006/10/06/Rolling_Your_Own_Blog_Engine.aspx/),
I generally give up.

This weekend, I came to the conclusion that [Jekyll](http://jekyllrb.com/) is
probably the right idea.

So I cobbled something together in node.js to scrape the content from the MySql
database behind my original drupal-powered blog and pushed the results to
GitHub Pages.

However, I'm not going to continue to use GitHub pages, because I want better
access to the webserver logs (for 'page not found' errors, etc.), and I might
want to experiment with Jekyll plugins.

~~And, because a [blog should allow
comments](http://blog.codinghorror.com/a-blog-without-comments-is-not-a-blog/),
I've installed [Discourse](http://discourse.differentpla.net/).~~

----

#### Update: 20 Apr 2019

I _am_ going to continue with GitHub Pages; I use Google Analytics for 404 errors; the list of plugins supported by GitHub Pages is OK; I uninstalled Discourse; I'm at peace with not allowing comments.
