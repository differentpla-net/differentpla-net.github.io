---
title: "Symbol Store: How are EXE files stored?"
date: 2009-04-23T15:12:23.000Z
x-drupal-nid: 235
x-needs-review: 2009-04-23T15:12:23.000Z
---
If you're using the Microsoft Symbol Server to pull down symbols for Windows, or if you're using SYMSTORE.EXE for your own stuff, you'll see that EXE (and other binary files) are stored in a path similar to `C:\WebSymbols\user32.dll\4226015990000\user32.dll`.

Where did the magic number come from?

It's the timestamp and size of the file from the PE header:

<pre>dumpbin /headers user32.dll
...
FILE HEADER VALUES
...
        42260159 time date stamp Wed Mar 02 18:09:29 2005
...
OPTIONAL HEADER VALUES
...
           90000 size of image</pre>

The magic number is simply these two values catenated.

If you get a user-mode minidump, the size and timestamp are stored in the .MDMP file. From there, you can query the symbol server for the exact binary module that the computer was running.
