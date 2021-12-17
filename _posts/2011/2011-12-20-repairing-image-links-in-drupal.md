---
title: "Repairing image links in drupal"
date: 2011-12-20T15:52:56.000Z
x-drupal-nid: 269
x-needs-review: 2011-12-20T15:52:56.000Z
---
Somehow -- I'm not sure how, and I'm not exactly sure when -- my website (powered by drupal) lost all of its images. They were still there in the filesystem, and the rows were still in the `node` table, but they'd vanished from the actual pages.

Further investigation revealed that the `image` and `files` tables had mostly forgotten about those images. So, how to quickly fix this all up?

First, we need to get the list of images from the filesystem back into the `files` table:

`$ ls -1s --block-size=1 *.png | grep -v thumbnail | sed "s@\([0-9 ]*\) \(.*\)@insert into files(uid, filename, filepath, filemime, filesize, status, timestamp) values(1, '\2', '/path/to/files/images/\2', 'image/png', \1, 1, 0);@g"`
Paste the output from this into a mysql prompt, and repeat for .jpg files (changing the mime type to image/jpeg).

Then you need to patch up the `image` table:

`mysql> delete from image;
 mysql> insert into image(nid, fid, image_size) select distinct n.nid, f.fid, '_original' from node n join files f on n.title = f.filename where n.type='image';`
This is not a completely automatic process, and I had to delete a couple of duplicate records, and do some other minor fixing (and delete drupal and browser caches), but it pretty much got everything back together.

It seems to work for now, anyway.
