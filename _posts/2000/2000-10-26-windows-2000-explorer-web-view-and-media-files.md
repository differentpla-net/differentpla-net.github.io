---
title: "Windows 2000 Explorer Web View and Media Files"
date: 2000-10-26T16:29:00.000Z
tags: windows-2000
---
**Warning:** I've recently received a couple of emails stating that this
technique totally breaks Web View in Windows Explorer. Use it at your
own risk. Back up any files you change.

When using the Windows Explorer in Web View, Windows 2000 (and
presumably ME, but I've not checked), adds a preview window for media
files to the left hand pane.

This causes problems when the files in question are on the other end of
a Samba connection. In particular, you can't rename the files without
getting a sharing violation. In short, the Media Player preview window
has the file locked open, and Samba doesn't like it.

The fix? Just open the file `C:\WINNT\Web\folder.htt`, and remove the
following snippet (don't forget to back the file up, just in case):

```html
<span id=MediaPlayerSpan>
</span>
```

Note: You might experience general wierdness with your editor, because
the file is marked hidden.

Media Player also leaches onto the Search Results view. To fix it, open
the file `C:\WINNT\Web\fsresult.htt`, and find the line:

```js
var gWantMedia = true;
```

Change `true` to `false`. Again, back up the file.

I've also had problems with the image preview in the search results. To
fix this, find the line:

```js
if (item.Size)
```

Change it to:

```js
if (false && item.Size)
```
