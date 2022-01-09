---
title: "Which of my MP3 directories don't have a Folder.jpg file?"
date: 2007-09-08T16:39:22.000Z
---
There's probably a better way to do this, but here's what I came up with in a couple of minutes...
<pre>Get-ChildItem $mp3_root -recurse -include *.mp3 | 
Split-Path -parent | 
Get-Unique | 
ForEach-Object {
    $folder_jpg = Join-Path $_ Folder.jpg ;
    if (-not (Test-Path $folder_jpg)) { $_ + ": missing Folder.jpg" }
}</pre>