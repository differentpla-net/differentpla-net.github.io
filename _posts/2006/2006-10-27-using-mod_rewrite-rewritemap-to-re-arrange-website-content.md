---
title: "Using mod_rewrite, RewriteMap to re-arrange website content"
date: 2006-10-27T12:39:30.000Z
---

When [moving]({% post_url 2006/2006-10-06-differentplanet-is-moving %}) differentpla.net from one server to another, I
decided to move the content (currently served using drupal) from the root to `/content`. I also decided to use more
descriptive URLs.

But I didn't want to break too many external links.

Here's how I used Apache's `mod_rewrite` - specifically `RewriteMap` - to achieve this.

As explained in the linked page, I've moved differentpla.net from my Linux box at home to a Virtual Private Server
(VPS), hosted in the US.

I'll also be moving a few other websites from this machine. In order to better implement a shared installation of both
Gallery2 and drupal across those websites, I decided to move differentpla.net's content from the root to a subdirectory.

Along with this, I decided to make use of drupal's path remapping support to generate more descriptive URLs for the
content.

However, I don't particularly want to break external links to the website, so I looked into using mod_rewrite to help
with this.

It turns out that mod_rewrite has a feature called RewriteMap, which does exactly what I want.

To use it, I simply added the following section to
`/etc/apache2/sites-available/differentpla.net`:

    <IfModule mod_rewrite.c>
        RewriteEngine on

        RewriteLog /var/log/apache2/rewrite.log
        RewriteLogLevel 0

        # Anything matching /node/X or /node/view/X is remapped
        # via this text file, which is keyed on X
        RewriteMap txtmap txt:/home/www/sites/differentpla.net/map.txt

        RewriteCond %{REQUEST_URI}      ^/node/view/
        RewriteRule ^/node/view/(.*)    /content/${txtmap:$1} [R]

        RewriteCond %{REQUEST_URI}      ^/node/
        RewriteRule ^/node/(.*)         /content/${txtmap:$1} [R]
    </IfModule>

It's enclosed in a condition block, so that it only takes effect if mod_rewrite is enabled.

The first line turns on mod_rewrite. The next two control logging. Level 0 (zero) means that rewrite logging is off.

Then I define the RewriteMap. Currently this is a simple text file. mod_rewrite also has support for
[other types of data source](http://httpd.apache.org/docs/2.0/mod/mod_rewrite.html#rewritemap).

Then there are the two things I want to recognise. There are two different ways to access drupal content. You can go to
`/node/NNN` or `/node/view/NNN`. These two rules extract the NNN, look it up in the map, prefix it with `/content/`, and then
issue a redirect.

Everything else is passed through unchanged.

Here's a snippet from map.txt:

    1       welcome
    10      2002/03/rsync-empegs

So, you can see that `/node/1` or `/node/view/1` are mapped to `/content/welcome`; and that `/node/10` and `/node/view/10` are
mapped to `/content/2002/03/rsync-empegs`.
