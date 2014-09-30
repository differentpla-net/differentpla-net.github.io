---
title: Spelunking Cowboy
date: 2013-11-03T19:54:20Z
---
# Background

I'm learning Erlang at the moment, and one way to learn a new language is to
read existing programs written in that language.

At the moment, I'm poking through the code for Cowboy, an HTTP server, and
Ranch, the corresponding network acceptor library that it uses.

As I find interesting snippets, I'm going to blog them. I'm hoping that by
doing this:

1. I'll find better understanding of what's going on by needing to explain it
   in plain language.
2. Other people might find my explanations useful.

This isn't going to be a deep-dive into the Cowboy code (at least not yet), so
the order in which I present things might seem a little odd.

Also note that I'm going to egregiously trim down the context here; for the
full skinny, go and read the code on github.

For the list of posts on this topic, use the [cowboy-spelunking](/tag/cowboy-spelunking) tag.
