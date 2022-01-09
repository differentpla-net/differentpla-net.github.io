---
title: "Getting hold of the current directory in a batch file."
date: 2004-05-19T16:33:00.000Z
redirect_from: /node/view/255
---
A common UNIX shell idiom is to write things like `cwd=$(pwd)` to save
the current directory so that in case be used later when other things might
have changed it. For a long time I didn't think this was possible in Windows
batch files. It turns out that it is, but it isn't pretty and it does
generalise to getting the output of any command.

_**Edit:** Thanks to the anonymous people who pointed out that the standard
shell environment variable `%CD%` provides this functionality already.
I've left the rest of the article in place because it's still a useful idiom
for getting the output of any command into an environment variable._

People have gone to [great lengths](
http://www.skarnet.org/software/execline/backtick.html) to get this functionality on Windows but such techniques don't have the convenience of a simple variable assignment.

It turns out that it can be done using a side effect of the hoopy new for
statement. This functionality may be XP specific - I haven't checked on
anything older. I very much doubt it will work on anything older than Windows 2000.

Here's the code to get hold of the current directory:

    for /f %%i "delims=" in ('cd') do set cwd=%%i

Try `for /?` to get the full information but here's a brief description.
The `/f` flag invokes this hoopy new mode, `"delims="` indicates
that we don't want to treat spaces and tabs as delimiters and `('cd')`
specifies that we should run the `cd` command to get the current
directory.

_This post was originally posted by Mike, rather than Roger; I'm leaving it here for posterity (and because Raymond Chen's blog links to it...)_