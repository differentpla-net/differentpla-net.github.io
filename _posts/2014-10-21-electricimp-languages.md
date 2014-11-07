---
title: Which programming languages do we use at Electric Imp?
date: 2014-10-21 10:57:12Z
---

This question just came up on Twitter:

> [@electricimp](https://twitter.com/electricimp) i am looking to do an
> internship with you, i was just wondering what would be the best programming
> languages for me to develop? 

-- Callum Orr (@OrrCallum) [12:15 PM - 20 Oct 2014](https://twitter.com/OrrCallum/status/524156992127250432)

As with so many things in computing, the correct answer is always "it depends".
If you're someone, like Callum, looking at an internship, it depends mostly on
what you want to get out of that internship.

**Note that opinions expressed herein are mine, not Electric Imp's.**

Based on a quick look through our source repositories, it would appear that we
primarily use the following languages:

 - C++
 - Erlang
 - Python
 - JavaScript
 - Squirrel
 - Objective-C
 - Java

I'm not entirely sure how to rank them, but each of these has its place.

For instance, the embedded improm is mostly written in C++, but makes use of a
large quantity of C code, such as eCos. So, if you want to work with that team,
you'd want to learn **C++**.

The back-end is [mostly written in
**Erlang**](http://blog.electricimp.com/post/85932940180/electric-imp-moves-to-erlang-programming-language),
but we know that Erlang programmers aren't exactly common, so we'd be looking
for someone who's familiar with concurrent programming and has experience of
another functional language.

We use **Python** extensively for [testing our
software](https://community.electricimp.com/blog/how-electric-imp-tests-software/),
both for the improm and for the back-end.

The [IDE](http://ide.electricimp.com) is written in **JavaScript**, using
jQuery, Backbone, handlebars, and a bunch of other stuff.

We also have some **node.js** stuff still kicking around, though that's generally
treated as legacy these days.

Our internal ops console is written using **Django** (**Python** again). Other
internal scripts and tools are written in whichever language seemed like a good
idea at the time. Some are written in **Python**, some in **node.js**, some in
**Ruby**. One in particular is written in **Go**.

The mobile apps and SDK are written in **Objective-C** (iOS) and **Java**
(Android).

The imp platform provides a **Squirrel** runtime. If you already know another
mainstream language, such as C++, C# or JavaScript, you'll be fine.

So, to recap, for an internship at Electric Imp, I would suggest that it makes
sense to learn some Python and some JavaScript. These are both reasonable
introductions to general-purpose programming languages; they both have wider
application than Squirrel.

If any of this sounds interesting to you -- internship or otherwise -- you
should check out our [jobs page](http://electricimp.com/aboutus/jobs/).
