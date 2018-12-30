---
title: "Release Process: Source Indexing using P4INDEX reports \"zero source files found\""
date: 2010-05-28T09:03:33.000Z
x-drupal-nid: 255
x-needs-review: 2010-05-28T09:03:33.000Z
---
When you use P4INDEX (or SSINDEX -System=P4), you might see "zero source files found". Check that your workspace doesn't have forward slashes in the name (e.g. D:/Source/depot/).

Why is this a problem? Well, P4INDEX runs `p4 have ...`, which returns, for example `//depot/whatever/file.cpp#42 - D:/Source/depot\whatever\file.cpp`.

The PDB contains files of the form `D:\Source\depot\whatever\file.cpp`. Note the backslashes. P4INDEX doesn't consider these equivalent, and so it can't get the revision details for the files named in the PDB.

Either ensure that your workspace uses backslashes, or hack on the p4.pm script to canonicalise the filenames before stashing them in the FILE_LOOKUP_TABLE dictionary.
