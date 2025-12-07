---
title: "Crafting Interpreters in Rust: Starting with LALRPOP"
short_title: Starting with LALRPOP
date: 2025-12-01T19:25:00Z
layout: series
series: crafting-interpreters-rust
tags: crafting-interpreters rust lox lalrpop december-adventure-2025
---

For the parser, I'm going to use [LALRPOP](https://crates.io/crates/lalrpop). I used it in my previous attempt at this,
and I liked it.

> LALRPOP is a Rust parser generator framework with _usability_ as its primary goal.

I'm going to start with a brand new Rust application, add LALRPOP to it, and try it out with a simple grammar.

## Create a new application

```sh
cargo new rlox
```

## Add LALRPOP

The ["Quick Start Guide"](https://lalrpop.github.io/lalrpop/quick_start_guide.html) almost has you covered. We need to
add the `features` as well, otherwise we get an error later about a missing lexer:

```toml
# Cargo.toml
[dependencies]
lalrpop-util = { version = "0.22.2", features = ["lexer", "unicode"] }

[build-dependencies]
lalrpop = "0.22.2"
```

```rust
// build.rs
fn main() {
    lalrpop::process_src().unwrap();
}
```

## Add a simple grammar

For now, we'll add a grammar that parses `true` or `false` as a boolean:

```
// lox.lalrpop
grammar;

pub Bool: bool = {
    "true" => true,
    "false" => false,
};
```

Why start with booleans? Because they're two of Lox's `primary` rules from [the book](https://craftinginterpreters.com/parsing-expressions.html):

```
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
```

## Using our parser

```rust
// main.rs
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(lox);

fn main() {
    let parser = lox::BoolParser::new();

    println!("{:?}", parser.parse("true"));
    println!("{:?}", parser.parse("false"));
}
```

```sh
$ cargo run
Ok(true)
Ok(false)
```

Looks good. We've got a simple application that successfully parses `true` and `false` tokens. In the next post, I'll
add other primitive types, such as numbers.
