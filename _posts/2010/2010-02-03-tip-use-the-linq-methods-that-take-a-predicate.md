---
title: "Tip: Use the LINQ methods that take a predicate"
date: 2010-02-03T09:33:09.000Z
---
Instead of this:

<pre>var author = (from a in dataContext.Authors where a.Id == authorId select a).Single();</pre>

...use this:

<pre>var author = dataContext.Authors.Single(a => a.Id == authorId);</pre>

Also consider whether you wanted `SingleOrDefault`.
