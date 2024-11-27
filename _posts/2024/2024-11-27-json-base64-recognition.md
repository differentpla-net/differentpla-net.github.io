---
title: Recognising base64-encodeed JSON
date: 2024-11-27T09:12:00Z
tags: 
---

If you see a chunk of text beginning with `eyJ`, it's probably base64-encoded JSON. Here's a demonstration.

Commonly when dealing with various APIs, you'll be confronted with base64-encoded data. If the base64 encoding starts
with `eyJ`, you can be pretty sure that it's base64-encoded JSON.

Here's a demonstration:

```sh
% echo '{"a' | base64
eyJhCg==
% echo '{"z' | base64
eyJ6Cg==
% echo '{"A' | base64
eyJBCg==
% echo '{"Z' | base64
eyJaCg==
```

Essentially, if the text begins with `{"`, then any ASCII character in the range 64-127 (that's upper- and lower-case
A-Z, plus a few bits of punctuation), it'll encode starting with `eyJ`. Since JSON keys are strings, and are almost
always alphabetic, this is what you'll most commonly see.