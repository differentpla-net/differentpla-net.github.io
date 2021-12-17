---
title: "FileInfo doesn't cache its contents"
date: 2004-07-23T08:21:00.000Z
x-drupal-nid: 74
x-needs-review: 2004-07-23T08:21:00.000Z
---
Imagine this code:

<pre>FileInfo oldInfo = new FileInfo(path);
RemoveID3TagsFromFile(path);
FileInfo newInfo = new FileInfo(path);
Trace.Assert(newInfo.Length < oldInfo.Length);</pre>

So, we've got some code that removes the ID3 tags from a file, thus shortening it. To check that it's working correctly, we use `FileInfo` to make sure that the new file is smaller than the old one.

The assert triggers anyway, even if the file is smaller. What's going on?

The answer: `FileInfo` doesn't actually look up the length until you ask for it, so `oldInfo.Length` and `newInfo.Length` are the same: the new length of the file.

Do this instead:

<pre>FileInfo oldInfo = new FileInfo(path);
long oldLength = oldInfo.Length;
RemoveID3TagsFromFile(path);
FileInfo newInfo = new FileInfo(path);
long newLength = newInfo.Length;
Trace.Assert(newLength < oldLength);</pre>