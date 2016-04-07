---
title: "Getting hold of the current directory in a batch file."
date: 2004-05-19T16:33:00.000Z
x-drupal-nid: 78
x-needs-review: 2004-05-19T16:33:00.000Z
---
A common UNIX shell idiom is to write things like <tt>cwd=`pwd`</tt> to save
the current directory so that in case be used later when other things might
have changed it. For a long time I didn't think this was possible in Windows
batch files. It turns out that it is, but it isn't pretty and it does
generalise to getting the output of any command.

_**Edit:** Thanks to the anonymous people who pointed out that the standard
shell environment variable <tt>%CD%</tt> provides this functionality already.
I've left the rest of the article in place because it's still a useful idiom
for getting the output of any command into an environment variable._

People have gone to [great lengths](
http://www.skarnet.org/software/execline/backtick.html) to get this functionality on Windows but such techniques don't have the convenience of a simple variable assignment.

It turns out that it can be done using a side effect of the hoopy new for
statement. This functionality may be XP specific - I haven't checked on
anything older. I very much doubt it will work on anything older than Windows
2000.

Here's the code to get hold of the current directory:

<pre>for /f %%i "delims=" in ('cd') do set cwd=%%i
</pre>

Try <tt>for /?</tt> to get the full information but here's a brief description.
The <tt>/f</tt> flag invokes this hoopy new mode, <tt>"delims="</tt> indicates
that we don't want to treat spaces and tabs as delimiters and <tt>('cd')</tt>
specifies that we should run the <tt>cd</tt> command to get the current
directory.
