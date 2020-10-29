---
title: "Conflicting 'lib' target"
date: 2004-01-09T10:39:00.000Z
tags: jam
x-drupal-nid: 117
x-needs-review: 2004-01-09T10:39:00.000Z
---
empeg's source tree has a directory called `lib`, in which the core libraries used by all of our products live. Unfortunately, this conflicts with one of the included pseudo-targets that jam uses.

The fix is relatively simple. You need to edit the included `Jambase` file, and rename every mention of `lib` to something else, e.g. `libs`.

Lines 552-554:

<div class="before">
<pre>DEPENDS all : shell files lib exe obj ;
DEPENDS all shell files lib exe obj : first ;
NOTFILE all first shell files lib exe obj dirs clean uninstall ;
</pre>

</div>

...change this to...
<div class="after">
<pre>DEPENDS all : shell files **libs** exe obj ;
DEPENDS all shell files **libs** exe obj : first ;
NOTFILE all first shell files **libs** exe obj dirs clean uninstall ;
</pre>

</div>

Lines 827-830:
<div class="before">
<pre>	else
	{
	    DEPENDS lib : $(_l) ;
	}
</pre>

</div>

...change this to...
<div class="after">
<pre>	else
	{
	    DEPENDS **libs** : $(_l) ;
	}
</pre>

</div>

You should probably also change the comments at lines 41-52 that refer to 'lib'.
This then requires that you use your new `Jambase` instead of the included one. You have two choices:

1.  Use the `-f` switch to `jam` to tell jam where to find an alternate Jambase file. This is the simplest, but requires more typing.
2.  Recompile jam, including the new file. This is relatively simple. Copy the edited `Jambase` into the source directory for jam, and rebuild it.
