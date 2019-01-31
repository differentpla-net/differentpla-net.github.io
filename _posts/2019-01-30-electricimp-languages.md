---
title: Which programming languages do we use at Electric Imp?
date: 2019-01-30 14:53
---

I last posted on this topic about [4 years ago]({% post_url 2014-10-21-electricimp-languages %}). It's time for an update.

**Note that opinions expressed herein are mine, not Electric Imp's.**

Based on a quick look through our source repositories, it would appear that we have code in the following languages:

 - bash -- for various scripting tasks.
 - C++ --  for impOS and the agent runtime.
 - Clojure -- a one-off tool (runs as an AWS Lambda), because why not?
 - Erlang -- most of the "core" backend.
 - Elixir -- some of the "core" backend.
 - Expect -- scripting.
 - Go -- various microservices.
 - Java -- BlinkUp SDK for Android.
 - JavaScript -- impCentral (the IDE).
 - Lua -- nginx integration.
 - Node.js -- the API, some microservices.
 - Objective-C -- BlinkUp SDK for iOS.
 - Perl -- various scripts.
 - Python -- system tests, ops tools.
 - RobotFramework -- system tests.
 - Ruby -- we use chef, other ops tools.
 - Squirrel -- this is what customers use on our platform.
 - Swift -- BlinkUp SDK for iOS
 - Tcl -- we use Vera to enforce C++ conventions; it's scripted using Tcl, and we have some custom scripts.
 - TypeScript -- internal tools.
