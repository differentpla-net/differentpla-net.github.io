---
title: Dependency my_app is specified as a dependency but is not reachable by the system
date: 2014-07-07T13:26:04Z
tags: erlang
---
While using `erlang.mk` and `relx` to build a newly-created Erlang application, I got the following error:

    Failed to solve release:
     Dependency my_app is specified as a dependency but is not reachable by the system.

It turns out that I was missing the `modules` term in the `src/my_app.app.src` file.
`erlang.mk` needs it to be present (but empty), or it won't write it to the `ebin/my_app.app` file.
`relx` needs it to be in the `ebin/my_app.app` file, otherwise it fails with the above error.

I'd used `rebar` to create the application skeleton, and it leaves this out.

The solution is simply to add it:

    {application, my_app,
     [
      % etc.
      {modules, []},
      % etc.
     ]}.

