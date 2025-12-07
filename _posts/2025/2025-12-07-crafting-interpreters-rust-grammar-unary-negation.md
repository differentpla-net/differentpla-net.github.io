---
title: "Crafting Interpreters in Rust: Parsing Unary Negation"
short_title: Parsing unary negation
date: 2025-12-07T15:00:00Z
layout: series
series: crafting-interpreters-rust
tags: crafting-interpreters rust lox lalrpop december-adventure-2025
---

In the last post, we implemented unary-not. In this one, we'll implement the other unary operator: negation.

As usual, we'll start with a test:

```rust
    #[test]
    fn negation() {
        let parser = lox::ExpressionParser::new();
        let expr = parser.parse("-42").unwrap();
        assert_eq!(expr, Expression::Nil);
    }
```

Now -- obviously -- this isn't going to pass, but it'll prompt us through the steps to get it to compile.

First we have to update the grammar to add `"-"`:

```
Unary = {
    "!" <expr: Unary> => Expression::Not(Box::new(expr)),
    "-" <expr: Unary> => Expression::Negate(Box::new(expr)),
    Primary
};
```

Then we update the `Expression` type:

```rust
#[derive(Debug, PartialEq)]
pub enum Expression<'input> {
    // ...
    Negate(Box<Expression<'input>>),
}
```

Then we can update the test so that it passes:

```rust
    #[test]
    fn negation() {
        let parser = lox::ExpressionParser::new();
        let expr = parser.parse("-42").unwrap();
        assert_eq!(expr, Expression::Negate(Box::new(Expression::Number(42.0))));
    }
```

...and we might as well add another assertion for our evaluator:

```rust
    #[test]
    fn negation() {
        let parser = lox::ExpressionParser::new();
        let expr = parser.parse("-42").unwrap();
        assert_eq!(expr, Expression::Negate(Box::new(Expression::Number(42.0))));
        assert_eq!(evaluate(expr), Expression::Number(-42.0));
    }
```

Note that, as it stands, this grammar doesn't allow for a C-style decrement operator, `--x`, because we parse it as a
double-negation. This is fine: Lox doesn't define increment or decrement operators.

And now we also need to update the `evaluate` function:

```rust
use crate::ast::*;

pub fn evaluate(expr: Expression) -> Expression {
    match expr {
        Expression::Not(v) => { /* ... */ },
        Expression::Negate(v) => {
            let v = evaluate(*v);
            match v {
                Expression::Number(n) => Expression::Number(-n),
                any => panic!("{:?}", any)
            }
        },

        any => any
    }
}
```

And the new test passes, so we've done both unary operators. In the next part, we'll take a look at the first binary
operators: division and multiplication.
