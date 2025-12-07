---
title: "Crafting Interpreters in Rust: Adding numbers to the grammar"
short_title: Parsing numbers
date: 2025-12-02T17:23:00Z
layout: series
series: crafting-interpreters-rust
tags: crafting-interpreters rust lox lalrpop december-adventure
---

So far, we've got a grammar that can parse `true`, `false` and `nil` literals. In this post, we'll add the ability to
parse numbers.

Note: Lox uses floating-point numbers.

Let's start with a test:

```rust
    #[test]
    fn positive_numbers() {
        let parser = lox::PrimaryParser::new();
        assert_eq!(parser.parse("0").unwrap(), Primary::Number(0f64));
        assert_eq!(parser.parse("42").unwrap(), Primary::Number(42f64));
        assert_eq!(parser.parse("12.34").unwrap(), Primary::Number(12.34));
    }
```

You'll notice that we don't support negative numbers. We'll deal with those later, probably by defining the unary minus
operator.

The test fails to compile, because we've not defined `Primary::Number`, so we need to do that:

```rust
#[derive(Debug, PartialEq)]
pub enum Primary {
    Nil,
    Bool(bool),
    Number(f64),
}
```

Now we get a failure compiling the tests, because -- in Rust -- `0`, `42`, etc. are _integer_ literals. We need to add
`f64` suffixes to them: `0f64`, etc..

It compiles, but the test fails with the following:

```
called `Result::unwrap()` on an `Err` value: InvalidToken { location: 0 }
```

The LALRPOP-generated grammar doesn't recognise whatever's at location 0, and has returned an `InvalidToken` error.

To fix that, we need to add some more grammar:

```
use std::str::FromStr;                  // add this
use crate::ast::*;

grammar;

pub Primary: Primary = {
    Nil => Primary::Nil,
    Bool => Primary::Bool(<>)
    Number => Primary::Number(<>),      // add this
};

// ...

// `Number` is 1 or more decimal digits, optionally followed by a decimal point and one or more decimal digits.
// In particular -- unlike Rust -- `.1` and `1.` are _not_ valid `Number` tokens.
Number: f64 = r"[0-9]+(\.[0-9]+)?" => f64::from_str(<>).unwrap();
```

That comment is worth drawing attention to:
- I'm not entirely clear that this conforms to the book's definition of a number.
- For floating-point literals, we require at least one decimal digit before and after the decimal point. That is: `0.0`
  is fine, but `.0` and `0.` are not.

Let's add negative tests for those cases:

```rust
    #[test]
    fn not_numbers() {
        let parser = lox::PrimaryParser::new();
        assert!(parser.parse(".0").is_err());
        assert!(parser.parse("0.").is_err());
    }
```

These tests aren't great: they don't check the content of the error; but they'll do for now.

We've added numbers to the grammar. In the next post, we'll add strings.
