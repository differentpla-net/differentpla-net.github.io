---
title: "Forwarding addresses to another account using vpopmail"
date: 2004-03-11T11:03:00.000Z
x-drupal-nid: 85
x-needs-review: 2004-03-11T11:03:00.000Z
---
Part 7 of [Installing qmail and vpopmail](/node/view/165).

## Forwarding

Some of the email addresses on `peculiar` are "vanity addresses". The people concerned don't have user accounts on the box and the email is forwarded somewhere else.

This is easy to configure using `valias`:

<pre># /home/vpopmail/bin/valias -i neil@somewhere.else.com aliens@differentpla.test
# /home/vpopmail/bin/valias -i bob@somewhere.else.com aliens@differentpla.test</pre>

This sets up the email address `aliens@differentpla.test` so that email is forwarded to `neil` and `bob` on a different server.
To check that it's set up correctly:

<pre># /home/vpopmail/bin/valias aliens@differentpla.test
aliens@differentpla.test -> neil@somewhere.else.com
aliens@differentpla.test -> bob@somewhere.else.com</pre>

`valias` works by putting a `.qmail-aliens` file in the relevant domain directory.

Next: [Installing ezmlm with vpopmail](/node/view/174).
