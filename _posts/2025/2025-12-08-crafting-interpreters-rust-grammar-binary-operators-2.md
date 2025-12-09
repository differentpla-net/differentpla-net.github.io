---
title: "Crafting Interpreters in Rust: Parsing Binary Operators, part 2"
short_title: Parsing binary operators, part 2
date: 2025-12-08T18:27:00Z
layout: series
series: crafting-interpreters-rust
tags: crafting-interpreters rust lox lalrpop december-adventure-2025
---

At the end of the previous post, we could parse division and multiplication operators. Because we used an LALRPOP macro,
it should be relatively simple to add addition and subtraction operators.

## Addition

As usual, we start with a test:

```rust
    #[test]
    fn addition() {
        let parser = lox::ExpressionParser::new();
        let expr = parser.parse("12 + 3").unwrap();
        assert_eq!(
            expr,
            Expression::BinaryOperation {
                left: Box::new(Expression::Number(12.0)),
                op: BinaryOperator::Add,
                right: Box::new(Expression::Number(3.0))
            }
        );
        assert_eq!(evaluate(expr), Expression::Number(15.0));
    }
```

Then we update the `BinaryOperator` AST enum:

```rust
#[derive(Debug, PartialEq)]
pub enum BinaryOperator {
    Divide,
    Multiply,
    Add,
    Subtract,
}
```

Then we update the grammar. The "Crafting Interpreters" book calls this `term`...

```
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
```

...so we'll do the same:

```
pub Expression = Term;

// term : term ( "-" | "+" ) factor
//      | factor
Term = BinaryOperation<Term, TermOp, Factor, Factor>;

TermOp: BinaryOperator = {
    "-" => BinaryOperator::Subtract,
    "+" => BinaryOperator::Add
};
```

We also need to fix the `evaluate` function:

```rust
        // ...
        Expression::BinaryOperation { left, op, right } => {
            let left = evaluate(*left);
            let right = evaluate(*right);
            match (left, right) {
                (Expression::Number(left), Expression::Number(right)) => match op {
                    BinaryOperator::Divide => Expression::Number(left / right),
                    BinaryOperator::Multiply => Expression::Number(left * right),
                    BinaryOperator::Add => Expression::Number(left + right),
                    BinaryOperator::Subtract => todo!(),
                },
                any => panic!("{:?}", any),
            }
        }
        // ...
```

And all of the tests pass, so we can immediately move on to...

## Subtraction

```rust
    #[test]
    fn subtraction() {
        let parser = lox::ExpressionParser::new();
        let expr = parser.parse("12 - 3").unwrap();
        assert_eq!(
            expr,
            Expression::BinaryOperation {
                left: Box::new(Expression::Number(12.0)),
                op: BinaryOperator::Subtract,
                right: Box::new(Expression::Number(3.0))
            }
        );
        assert_eq!(evaluate(expr), Expression::Number(9.0));
    }
```

```rust
        // ...
        Expression::BinaryOperation { left, op, right } => {
            let left = evaluate(*left);
            let right = evaluate(*right);
            match (left, right) {
                (Expression::Number(left), Expression::Number(right)) => match op {
                    BinaryOperator::Divide => Expression::Number(left / right),
                    BinaryOperator::Multiply => Expression::Number(left * right),
                    BinaryOperator::Add => Expression::Number(left + right),
                    BinaryOperator::Subtract => Expression::Number(left - right),
                },
                any => panic!("{:?}", any),
            }
        }
        // ...
```
