---
title: "Crafting Interpreters in Rust: Parsing groups"
short_title: Parsing groups
date: 2025-12-10T12:50:00Z
layout: series
series: crafting-interpreters-rust
tags: crafting-interpreters rust lox lalrpop december-adventure-2025
---

We're almost done with expressions. Next we add grouping with parentheses.

Recall that `primary` is defined in the book as follows:

```
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
```

We need to implement that `"(" expression ")"` bit.

The test looks like this:

```rust
    #[test]
    fn grouping_multiply_divide() {
        let parser = lox::ExpressionParser::new();
        let expr = parser.parse("((6 * 3) / (2 * 3))").unwrap();
        assert_eq!(
            expr,
            Expression::BinaryOperation {
                left: Box::new(Expression::BinaryOperation {
                    left: Box::new(Expression::Number(6.0)),
                    op: BinaryOperator::Multiply,
                    right: Box::new(Expression::Number(3.0))
                }),
                op: BinaryOperator::Divide,
                right: Box::new(Expression::BinaryOperation {
                    left: Box::new(Expression::Number(2.0)),
                    op: BinaryOperator::Multiply,
                    right: Box::new(Expression::Number(3.0))
                })
            }
        );
        assert_eq!(evaluate(expr), Expression::Number(3.0));
    }
```

The implementation looks like this (in the `lox.lalrpop` file):

```
// primary : NUMBER | STRING | "true" | "false" | "nil"
//         | "(" expression ")" ;
Primary: Expression<'input> = {
    Nil => Expression::Nil,
    Bool => Expression::Bool(<>),
    Number => Expression::Number(<>),
    String => Expression::String(<>),
    "(" <Expression> ")"
};
```

...and we're done. That's Lox's expressions parsed.

That's -- essentially -- the book to the end of Chapter 6. We've also implemented a bunch of the evaluator from Chapter
7.