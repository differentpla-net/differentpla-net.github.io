---
title: "Unable to open Web Project"
date: 2006-11-16T12:43:08.000Z
x-drupal-nid: 76
x-needs-review: 2006-11-16T12:43:08.000Z
---
<pre>Unable to open Web Project 'foo'.
The file path 'C:\wherever\foo' does not correspond to the URL 'http://localhost/foo'.
The two need to map to the same server location.</pre>

Some possible causes, and fixes.

When you attempt to open a web project in Visual Studio, it first attempts to confirm that the virtual directory actually maps to the directory from which you've opened it.

It does this by placing a temporary .htm file in the directory, and then it checks that this is available via the virtual directory in IIS. This ensures that the two refer to the same location.

If this fails, you'll get the error message above. If you're lucky, it might have a little bit of extra information, like "403 Forbidden".

Sometimes this is caused by broken permissions, by "broken" HTTP handlers -- e.g. that map the temporary file created by VS to somewhere else.

More often, though, Visual Studio is just f*cked up. If no other poking around resolves the issue, you should simply delete the `%USERPROFILE%\VSWebCache` directory.
