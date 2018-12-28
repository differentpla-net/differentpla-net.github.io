---
title: "Advanced Searches in emplode v2.0"
date: 2001-10-25T19:29:00.000Z
---
emplode v2.0 adds the ability to search your music database. It provides two methods of doing this: simple searches (which are hopefully so simple that I won't explain them here), and advanced searches.

## Syntax

```
search  ::= conj [or conj]*
conj    ::= term [and term]*
term    ::= item [relop item]
item    ::= literal | tag | "(" search ")" | not item
literal ::= dquote-char (alpha|numeric|punctuation)* dquote-char
          | numeric+
          | "true" | "false"
tag     ::= alpha*
```

## Tags

Essentially, anything that appears in a *1 file is fine. We also allow the following pseudo-tags:

* fid
* refs
* new
* changed
* marked
* drive
* play_count
* play_last
* skipped_count

Some of them return time_t values, which aren't terribly useful. We'll come up with a way to express these for matching later.

## Boolean Values

"true" and "false" are valid values. They evaluate as you'd expect. Strings evaluate to false if empty, true otherwise.

## Case-sensitivity

String comparisons are case-insensitive. Tag names are case-insensitive, except for the pseudo-tags. This is a bug.
