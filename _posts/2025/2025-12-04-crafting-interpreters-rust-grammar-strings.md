---
title: "Crafting Interpreters in Rust: Adding strings to the grammar"
short_title: Parsing strings
date: 2025-12-04T08:23:00Z
layout: series
series: crafting-interpreters-rust
tags: crafting-interpreters rust lox lalrpop december-adventure-2025
---

We're implementing Lox's `primary` rule. So far, we've done booleans, `nil` and numbers. Let's move on to strings.

In the book, Lox's [`primary` rule](https://craftinginterpreters.com/parsing-expressions.html) looks like this:

```
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
```

We start with a test:

```rust
    #[test]
    fn strings() {
        let parser = lox::PrimaryParser::new();
        assert_eq!(parser.parse("\"\"").unwrap(), Primary::String(""));
        assert_eq!(parser.parse("\"hello\"").unwrap(), Primary::String("hello"));
    }
```

When I originally tried this exercise, I stored the parsed strings as Rust's `String` type. This time, I'll use string slices: `&str`. In `ast.rs`, that looks like this:

```rust
#[derive(Debug, PartialEq)]
pub enum Primary<'input> {
    Nil,
    Bool(bool),
    Number(f64),
    String(&'input str)
}
```

In the grammar, that looks like this (LALRPOP provides us with the `'input` lifetime):

```
String: &'input str = r#""[^"]*""#;
```

The regex matches a double quote, followed by anything that's not a dquote, followed by another dquote.

We use a raw string literal: `r#"...."#`. See
<https://doc.rust-lang.org/rust-by-example/std/str.html#literals-and-escapes>

Actually, we should go back and use that instead of the escapes in the test:

```rust
        assert_eq!(parser.parse(r#""""#).unwrap(), Primary::String(""));
        assert_eq!(parser.parse(r#""hello""#).unwrap(), Primary::String("hello"));
```

And now it compiles, but the test fails, because we're picking up the surrounding double quotes:

```
assertion `left == right` failed
  left: String("\"\"")
 right: String("")
```

We need to trim those in the grammar. For the first step, we'll name the symbol, using `<name:pattern>`, and use a custom action that just returns the value:

```
String: &'input str = <s:r#""[^"]*""#> => s;
```

The test still fails, so we update the action to trim the surrounding quotes:

```
String: &'input str = <s:r#""[^"]*""#> => &s[1..s.len() - 1];
```

And then we can add some explanatory comments.

```
// A literal string is a quote, followed by zero-or-more non-quote characters, followed by a terminating quote.
// We don't want the surrounding quotes. We use a string slice.
// We don't support escape characters or Rust-style raw string literals.
String: &'input str = <s:r#""[^"]*""#> => &s[1..s.len() - 1];
```

And now the tests all pass.

The remaining thing in `primary` is `expression`, which is significantly more complicated; we'll start with that in the
next post.
