---
title: "Crafting Interpreters in Rust: Adding nil to the grammar"
short_title: Parsing nil
date: 2025-12-01T20:07:00Z
layout: series
series: crafting-interpreters-rust
tags: crafting-interpreters rust lox lalrpop december-adventure-2025
---

In the previous post, we wrote a simple grammar that parses `true` or `false` as a boolean:

```
// lox.lalrpop
grammar;

pub Bool: bool = {
    "true" => true,
    "false" => false,
};
```

In the book, Lox's [`primary` rule](https://craftinginterpreters.com/parsing-expressions.html) looks like this:

```
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
```

So, ignoring `expression` for now, and starting with the easiest, let's add `nil`.

## Introducing `Primary`

First, though, we need to introduce the `Primary` type:

```rust
// ast.rs
#[derive(Debug)]
pub enum Primary {
    Bool(bool)
}
```

Then we can add it to the grammar, with `Bool` as one of its variants:

```
use crate::ast::*;

grammar;

pub Primary: Primary = {
    Bool => Primary::Bool(<>)
};

Bool: bool = {
    "true" => true,
    "false" => false,
};
```

We also have to change `main.rs` to look like this:

```rust
// main.rs
use lalrpop_util::lalrpop_mod;

lalrpop_mod!(lox);

mod ast;

fn main() {
    let parser = lox::PrimaryParser::new();

    println!("{:?}", parser.parse("true"));
    println!("{:?}", parser.parse("false"));
}
```

Now when we run it, we get the following:

```sh
$ cargo run
Ok(Bool(true))
Ok(Bool(false))
```

## Tests

This ad-hoc testing is making me feel slightly grimy, so I'm going to write some initial tests, and then do the rest of
this as TDD.

```rust
// main.rs

// ...

#[cfg(test)]
mod tests {
    #[test]
    fn meh() {
        assert!(false)
    }
}
```

Always start with a failing test. I may have taken that a bit literally. This one's just to make sure that I'm running
`cargo test` properly:

```sh
$ cargo test
test tests::meh ... FAILED
```

Let's replace it with something more sensible:

```rust
#[cfg(test)]
mod tests {
    use crate::*;
    use crate::ast::*;

    #[test]
    fn true_() {
        let parser = lox::PrimaryParser::new();
        assert_eq!(parser.parse("true").unwrap(), Primary::Bool(true));
    }

    #[test]
    fn false_() {
        let parser = lox::PrimaryParser::new();
        assert_eq!(parser.parse("false").unwrap(), Primary::Bool(false));
    }
}
```

To get this to compile, we need to add `PartialEq` to `Primary`:

```rust
#[derive(Debug, PartialEq)]
pub enum Primary {
    Bool(bool)
}
```

## Adding `nil`

Those pass, so we can add a failing test for `nil`:

```rust
    #[test]
    fn nil() {
        let parser = lox::PrimaryParser::new();
        assert_eq!(parser.parse("nil").unwrap(), Primary::Nil);
    }
```

That requires us to add `Primary::Nil`:

```rust
#[derive(Debug, PartialEq)]
pub enum Primary {
    Nil,
    Bool(bool)
}
```

And we need to update the grammar:

```
// ...

pub Primary: Primary = {
    Nil => Primary::Nil,
    Bool => Primary::Bool(<>)
};

Nil = "nil";

// ...
```

And now the tests pass:

```sh
$ cargo test
running 3 tests
test tests::nil ... ok
test tests::false_ ... ok
test tests::true_ ... ok

test result: ok. 3 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s
```

We've extended the grammar to include the `nil` literal. In the next post, we'll look at adding numbers to the grammar.
