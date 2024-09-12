---
title: Removing comments from JSON
date: 2024-09-12T08:36:00Z
tags: json
---

Yes, I know that JSON files don't have comments. I've got some JSON files with comments in them. Here's a quick way to strip the comments.

```sh
sed 's,//.*,,' < file-containing-comments.json | json-tool-with-strong-opinions
```
