---
title: "Forwarding addresses to another account using vpopmail"
date: 2004-03-11T11:03:00.000Z
redirect_from: /node/view/173
tags: qmail
layout: series
series: qmail
---

## Forwarding

Some of the email addresses on `peculiar` are "vanity addresses". The people concerned don't have user accounts on the box and the email is forwarded somewhere else.

This is easy to configure using `valias`:

```
# /home/vpopmail/bin/valias -i neil@somewhere.else.com aliens@differentpla.test
# /home/vpopmail/bin/valias -i bob@somewhere.else.com aliens@differentpla.test
```

This sets up the email address `aliens@differentpla.test` so that email is forwarded to `neil` and `bob` on a different server.
To check that it's set up correctly:

```
# /home/vpopmail/bin/valias aliens@differentpla.test
aliens@differentpla.test -> neil@somewhere.else.com
aliens@differentpla.test -> bob@somewhere.else.com
```

`valias` works by putting a `.qmail-aliens` file in the relevant domain directory.
