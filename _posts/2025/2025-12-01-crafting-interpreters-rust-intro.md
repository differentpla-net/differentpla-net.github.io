---
title: "Crafting Interpreters in Rust: Introduction"
short_title: Introduction
date: 2025-12-01T19:14:00Z
layout: series
series: crafting-interpreters-rust
tags: crafting-interpreters rust lox lalrpop december-adventure-2025
---

For my `#DecemberAdventure`, I'm going to have another go at working through ["Crafting
Interpreters"](https://craftinginterpreters.com/) in Rust. The last time I tried this, I got to the end of Chapter 10.

However, it's my opinion that the lexing and parsing part of language implementation is the least interesting bit. It's
been done to death by every tutorial out there, so I'm going to skip that part entirely and write a parser using
[LALRPOP](https://crates.io/crates/lalrpop).

I did this last time, but when I look back at the grammar, I don't understand any of it. This time, I'm going to write
it down in this series of blog posts, so that I'll remember for next time.

{% include _series_toc.html %}
