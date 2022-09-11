---
title: What is this?
date: 2013-02-06T21:25:00Z
tags: blogging
---

<div class="callout callout-info" markdown="span">
**2022-09-11:** This blog continues to be hosted on GitHub Pages; this page is of historical interest only. If that.
</div>

I got fed up with my existing blog, so I decided to write a new blog engine, as a way to learn some new tech.

I do this every two or three years: spend a few hours playing with new technology, and then getting distracted and moving on to something else. It's a good way to learn new technology though.

With this in mind, this blog engine is going to use all of the latest shiny:

* [node.js](http://nodejs.org/), using [Express](http://expressjs.com/).
* Some form of document database. Either [MongoDB](http://www.mongodb.org/) or [CouchDB](http://couchdb.apache.org/).
* Twitter [Bootstrap](http://twitter.github.com/bootstrap/) for the styling.
* I'm also considering running it on [EC2](http://aws.amazon.com/ec2/), rather than on my PC at home.
* I might even throw in some [KnockoutJS](http://knockoutjs.com/) or similar.
* Unfortunately, I can't particularly see a niche for [socket.io](http://socket.io/), so I'll probably leave that out.

The source code is up on GitHub: [http://github.com/rlipscombe/blog](http://github.com/rlipscombe/blog).
