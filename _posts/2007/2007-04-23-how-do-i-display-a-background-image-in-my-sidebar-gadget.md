---
title: "How do I display a background image in my sidebar gadget?"
date: 2007-04-23T08:57:13.000Z
x-drupal-nid: 168
x-needs-review: 2007-04-23T08:57:13.000Z
---
When your gadget is docked, it displays a background image, which is allowed to be alpha-transparent.

This is controlled by the CSS for your gadget:

<pre>body {
    background-image: url(GadgetBackground.png);
    background-repeat: no-repeat;
}</pre>

Your background should be 130 pixels wide when docked, but the height can vary. The size is controlled by another CSS rule:

<pre>body.docked {
    width: 130px;
    height: 113px;
}</pre>

Note that this assumes that you're using different class names for the docked and undocked state of your gadget.

The background image should be a .PNG file (because you'll want alpha-transparency).