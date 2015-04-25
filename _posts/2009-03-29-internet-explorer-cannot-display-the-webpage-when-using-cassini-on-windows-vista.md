---
title: "\"Internet Explorer cannot display the webpage\" when using Cassini on Windows Vista"
date: 2009-03-29T09:08:33.000Z
x-drupal-nid: 231
x-needs-review: 2009-03-29T09:08:33.000Z
---
I was doing some ASP.NET MVC this weekend, and I couldn't get the project to start: all I got was "Internet Explorer cannot display the webpage", when using Visual Studio's built-in development web server (Cassini).

I think it's because Cassini doesn't support IPv6, and on Vista, "localhost" refers to the IPv6 address <tt>::1</tt>, rather than the IPv4 address <tt>127.0.0.1</tt>.

If you use http://127.0.0.1:12345/Home/, then it works fine.

To resolve this, you can go to the project settings and set the Start URL explicitly.