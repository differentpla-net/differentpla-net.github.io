---
title: "Escaping HTML so you can quote it in HTML"
date: 2004-02-26T10:13:00.000Z
x-drupal-nid: 61
x-needs-review: 2004-02-26T10:13:00.000Z
---
Quick tip: How to escape HTML text so that it can be quoted in another piece of HTML, e.g. in a `<pre>` tag.

<pre>cat index.html | \
    sed 's/&/\&amp;/' | \
    sed 's/"/\&quot;/' | \
    sed 's/</\&lt;/' | \
    sed 's/>/\&gt;/'
</pre>

There's probably a simpler way to do this, but it works for me, so I'm putting it here before I forget.