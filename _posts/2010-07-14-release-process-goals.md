---
title: "Release Process: Goals"
date: 2010-07-14T13:33:06.000Z
x-drupal-nid: 259
x-needs-review: 2010-07-14T13:33:06.000Z
---
One of the many things that my former company, 1E, does well is its build and release process. That got me thinking: what makes a good build process?

[The Joel Test](http://www.joelonsoftware.com/articles/fog0000000043.html) touches on this in item #2 (Can you make a build in one step?) and item #3 (Do you make daily builds?), and – to a lesser extent – item #1 (Do you use source control?). But he doesn’t go into any detail about why...

I think that there are 4 goals to keep in mind when designing a build and release process:

1.  A [new developer](/content/2010/07/release-process-new-developer) must be able to walk up to a new PC, get the latest source code and build it in a single step.
2.  When a developer commits changes to the project, they must be relatively certain that they’ve not broken anything.
3.  When you’re ready to release something to your customers, you should be able to do this in a single step.
4.  When a customer support case comes in, the developer responsible must be able to identify exactly which release the customer has, and exactly what went into that release.
