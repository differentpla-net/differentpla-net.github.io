---
title: "Crafting Interpreters in Rust: Parsing Binary Operators, part 3"
short_title: Parsing binary operators, part 3
date: 2025-12-10T10:00:00Z
layout: series
series: crafting-interpreters-rust
tags: crafting-interpreters rust lox lalrpop december-adventure-2025
---

So far, we implemented parsing for numbers, strings, booleans, nil, unary operators `!` and `-`, multiplication and
division, and addition and subtraction. Next on the list are comparison operators (greater-than, less-than, etc.) and
equality operators.

As a reminder, the grammar for expressions, from the book, looks like this:

```
expression     → equality ;
equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
```

## Comparison operators

As before, I wrote the tests first, but you'll have to take my word for it. They're squashed into
[commit 7512035056](https://codeberg.org/rlipscombe/rlox/commit/751203505628ea5ac310899a21320fa9543bf5b7).

Here's an example:

```rust
    #[test]
    fn compare_gt_true() {
        let parser = lox::ExpressionParser::new();
        let expr = parser.parse("12 > 3").unwrap();
        assert_eq!(
            expr,
            Expression::BinaryOperation {
                left: Box::new(Expression::Number(12.0)),
                op: BinaryOperator::Gt,
                right: Box::new(Expression::Number(3.0))
            }
        );
        assert_eq!(evaluate(expr), Expression::Bool(true));
    }
```

The implementation is relatively simple, now we've got our `BinaryOperation` macro. It looks like this:

```
pub Expression = Comparison;

// comparison : comparison ( ">" | ">=" | "<" | "<=" ) term
//            | term
Comparison = BinaryOperation<Comparison, ComparisonOp, Term, Term>;

ComparisonOp: BinaryOperator = {
    ">" => BinaryOperator::Gt,
    ">=" => BinaryOperator::Gte,
    "<" => BinaryOperator::Lt,
    "<=" => BinaryOperator::Lte,
};
```

...and we add `Gt`, `Gte`, etc. members to the `BinaryOperator` enum. Implementing `evaluate` is relatively simple, too:

```rust
    // ...
    BinaryOperator::Gt => Expression::Bool(left > right),
    BinaryOperator::Gte => Expression::Bool(left >= right),
    BinaryOperator::Lt => Expression::Bool(left < right),
    BinaryOperator::Lte => Expression::Bool(left <= right),
```                    

## Equality operators

These are also relatively easy; see [commit f2031aa4f9](https://codeberg.org/rlipscombe/rlox/commit/f2031aa4f92bc30550ab302b0b3f6347450c0e78).

Here's one of the tests:

```rust
    #[test]
    fn equal_when_equal() {
        let parser = lox::ExpressionParser::new();
        let expr = parser.parse("12 == 12").unwrap();
        assert_eq!(
            expr,
            Expression::BinaryOperation {
                left: Box::new(Expression::Number(12.0)),
                op: BinaryOperator::Eq,
                right: Box::new(Expression::Number(12.0))
            }
        );
        assert_eq!(evaluate(expr), Expression::Bool(true));
    }
```

And the parser implementation looks like this (we add `Eq` and `Ne` to `BinaryOperator`):

```
pub Expression = Equality;

// equality : equality ( "!=" | "==" ) comparison
//          | comparison
Equality = BinaryOperation<Equality, EqualityOp, Comparison, Comparison>;

EqualityOp: BinaryOperator = {
    "==" => BinaryOperator::Eq,
    "!=" => BinaryOperator::Ne,
};
```

The change to `evaluate` looks like this:

```rust
    // ...
    BinaryOperator::Eq => Expression::Bool(left == right),
    BinaryOperator::Ne => Expression::Bool(left != right),
```
