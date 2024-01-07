---
title: Logging Learnings
date: 2013-03-16 11:16:58
---

I've just spent the bulk of the morning attempting to understand a bug by
reading through some fairly large log files. I've come to the realisation that:

## Make them text-editor-friendly

Most log files are text. This means that, if you're tracking down a problem,
you'll probably want to search them, and you may want to edit them.

In particular, you may want to do a search+replace to pull out a bunch of lines
to do with a particular event or client. For this reason, **don't spread an
event across multiple lines**. You might think that it makes the file easier
for a human to read (and it does), but it makes it impossible to (for example)
delete all of the events that mention client 321 (because you're interested in
client 123), because you'll end up with hanging lines, as in the following
example:

    Client #123: Getting orders...
    Order #1: Foo
    Order #2: Bar
    Client #123: Returned 2 orders.
    Client #321: Getting orders...
    Order #3: Baz
    Client #321: Returned 1 order.

If you search-and-replace to delete "Client #321", you'll end up with the
following:

    Client #123: Getting orders...
    Order #1: Foo
    Order #2: Bar
    Client #123: Returned 2 orders.
    Order #3: Baz

If you tag-and-copy-to-new-file for "Client #123", you'll end up with the
following:

    Client #123: Getting orders...
    Client #123: Returned 2 orders.

Either way, you've lost information, and confused yourself. This makes it
harder to understand what's going on.

So, either include the context on every line:

    Client #321: Getting orders...
    Client #321: Order #3: Baz
    Client #321: Returned 1 order.

Or, don't spread your output over multiple lines.

## Consider using CSV (or JSON)

Yeah, your log files are human readable. That's great, provided you can find a
human prepared to read through a couple of million lines of log file.

Why don't you make them machine-readable? Then you'd allow the file to be
parsed and processed by a vast number of tools that already understand CSV (or
JSON).

## Don't use XML

Whatever you do, don't use XML. If you do, you've got two choices:

1. Output a malformed XML document that doesn't have a root element. If you do
   this, what's the point in using XML? I suppose that you could claim that
   each snippet was an individual XML fragment.
2. Close the root element after every write, and rewind to remove the `</root>`
   *before* every write. Say goodbye to your performance.

Also, it's massively verbose. Just use CSV or JSON.

## Consider using one file per client

Obviously, this isn't going to work for *your* web-scale application, because
you've got millions of clients visiting every second. However, if you've only
got a couple of dozen clients, consider using one log file per client.
