---
title: "Finding duplicate files"
date: 2002-04-01T23:00:00.000Z
x-drupal-nid: 105
x-needs-review: 2002-04-01T23:00:00.000Z
---
<div class="snippet">
    $ find . -type f -print0 |
    	xargs -0 md5sum > MD5SUMS
    $ sort < MD5SUMS | uniq -D -w 32 > DUPLICATES

</div>

...or the complicated way...

<div class="snippet">
    $ find . -type f -print0 |
    	xargs -0 md5sum > MD5SUMS
    $ sort < MD5SUMS | cut -d\  -f1 | uniq -D |
    	xargs -n1 -i grep {} MD5SUMS > DUPLICATES

</div>