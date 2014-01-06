---
layout: post
title: "epgsql: invalid_password"
date: 2013-12-31T14:55:00Z
tags: epgsql
---

I'm writing a simple web application in Erlang that uses
[epgsql](https://github.com/wg/epgsql) to talk to a PostgreSQL database. I keep
getting `invalid_password`, despite the fact that the user doesn't have a
password (and I'm passing ""). I can also log in by using `psql -U foo`.

Huh?

Turns out that the epgsql driver connects over TCP, rather than the unix domain
socket used by psql. You also need to add the following to `pg_hba.conf`:

    host    all     foo     127.0.0.1/32    trust

Sorted.

