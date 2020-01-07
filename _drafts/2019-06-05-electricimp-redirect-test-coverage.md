---
title: Erlang Coverage Trick
date: 2019-06-05 10:32
---

I'm refactoring a reasonably complicated piece of code in Electric Imp's
codebase -- it's the bit responsible for ensuring that a connected device
is redirected to the correct server.

We only have a couple of tests for it, which makes me uneasy, so I'll write
some more tests before I attempt anything.

To be confident that I've captured all of the expected functionality
in my new tests, I'm going to need some code coverage metrics.

The test environment is a collection of docker containers, running a cut-down
version of what's in production.

This means that I need to get coverage metrics from a running node.
