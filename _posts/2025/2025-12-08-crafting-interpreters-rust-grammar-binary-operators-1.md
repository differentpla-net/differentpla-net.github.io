---
title: "Crafting Interpreters in Rust: Parsing Binary Operators, part 1"
short_title: Parsing binary operators, part 1
date: 2025-12-09T17:09:00Z
layout: series
series: crafting-interpreters-rust
tags: crafting-interpreters rust lox lalrpop december-adventure-2025
---

In the previous post, we finished implementing parsing of unary operators. In this post, we'll take a look at the first
of the binary operators: division and multiplication.

## Division

As usual, we start with a test:

```rust
    #[test]
    fn division() {
        let parser = lox::ExpressionParser::new();
        let expr = parser.parse("12 / 3").unwrap();
        assert_eq!(
            expr,
            Expression::Divide {
                left: Box::new(Expression::Number(12.0)),
                right: Box::new(Expression::Number(3.0))
            }
        );
    }
```

Then we update the AST:

```rust
pub enum Expression<'input> {
    // ...
    Divide {
        left: Box<Expression<'input>>,
        right: Box<Expression<'input>>,
    }
}
```

Then we update the grammar:

```
pub Expression = Factor;

// factor : unary ( ( "/" | "*" ) unary )* ;
Factor = {
    <left: Unary> "/" <right:Unary> =>
        Expression::Divide {
            left: Box::new(left),
            right: Box::new(right)
        },
    Unary
};
```

The parser in the grammar is not quite the same as described in the comment. It actually implements the following:

```
factor : unary "/" unary
       | unary ;
```

We'll come back to this later in the post.

## Multiplication

We can parse multiplication in a very similar way to division:

```rust
    #[test]
    fn mulitplication() {
        let parser = lox::ExpressionParser::new();
        let expr = parser.parse("12 * 3").unwrap();
        assert_eq!(
            expr,
            Expression::Multiply {
                left: Box::new(Expression::Number(12.0)),
                right: Box::new(Expression::Number(3.0))
            }
        );
    }
```

```rust
pub enum Expression<'input> {
    // ...
    Multiply {
        left: Box<Expression<'input>>,
        right: Box<Expression<'input>>,
    },
}
```

```
Factor = {
    <left: Unary> "/" <right:Unary> => /* ... */
    <left: Unary> "*" <right:Unary> =>
        Expression::Multiply {
            left: Box::new(left),
            right: Box::new(right)
        },
    Unary
};
```

## Multiple operators

In most programming languages, including Lox, you can have more than one binary operator together. For example `2 * 3 * 4` is valid without parentheses and evaluates to 24.

This is expressed in the grammar in the book: `factor : unary ( ( "/" | "*" ) unary )* ;`

Let's add a test for that:

```rust
    #[test]
    fn many_multiplication() {
        let parser = lox::ExpressionParser::new();
        let expr = parser.parse("12 * 3 * 6").unwrap();
        assert_eq!(
            expr,
            Expression::Multiply {
                left: Box::new(Expression::Multiply {
                    left: Box::new(Expression::Number(12.0)),
                    right: Box::new(Expression::Number(3.0))
                }),
                right: Box::new(Expression::Number(6.0))
            }
        );
    }
```

It fails, because our parser isn't expecting another `*` character. We can fix it like this:

```
Factor = {
    <left: Factor> "/" <right:Unary> =>
        Expression::Divide {
            left: Box::new(left),
            right: Box::new(right)
        },
    <left: Factor> "*" <right:Unary> =>
        Expression::Multiply {
            left: Box::new(left),
            right: Box::new(right)
        },
    Unary
};
```

That is: we change `<left: Unary> "*" <right:Unary>` to `<left: Factor> "*" <right:Unary>`. This allows the left of the
operator to also be a `Factor`.

## Associativity

By doing this, we've implemented multiplication and division as left-associative operators.

A brief recap: operators that are associative are ones where `(a ~ b) ~ c` and `a ~ (b ~ c)` are equivalent.

In terms of arithmetic operators, addition and multiplication are associative. Subtraction and division are not
associative. `(6 / 3) / 2` is not the same as `6 / (3 / 2)`, for example.

If you want to evaluate `6 / 3 / 2`, you need to choose which of the above options you mean. In most programming
languages, the standard binary operators are left-associative: we parse the above as `(6 / 3) / 2`.

If we'd wanted a right-associative binary operator, we could have expressed it as the following instead:

```
Factor = {
    <left: Unary> "/" <right: Factor> =>
        Expression::Divide {
            left: Box::new(left),
            right: Box::new(right)
        },
    <left: Unary> "*" <right: Factor> =>
        Expression::Multiply {
            left: Box::new(left),
            right: Box::new(right)
        },
    Unary
};
```

## Macros

The grammar is all getting a bit long-winded. Fortunately, LALRPOP provides macros. See
<https://lalrpop.github.io/lalrpop/tutorial/006_macros.html>, which even gives us an example we could use directly.
Let's do something slightly different, though:

```
// factor : factor ( "/" | "*" ) unary
//        | unary ;
Factor = BinaryOperation<Factor, FactorOp, Unary, Unary>;

FactorOp: BinaryOperator = {
    "/" => BinaryOperator::Divide,
    "*" => BinaryOperator::Multiply
};

// This macro defines a binary operator...
// ... : left op right
//     | next
BinaryOperation<Left, Op, Right, Next> = {
    <left: Left> <op: Op> <right: Right> => Expression::BinaryOperation {
        left: Box::new(left), op, right: Box::new(right)
    },
    Next
}
```

We need to slightly fix up the tests. For example:

```rust
    #[test]
    fn multiplication() {
        let parser = lox::ExpressionParser::new();
        let expr = parser.parse("12 * 3").unwrap();
        assert_eq!(
            expr,
            Expression::BinaryOperation {
                left: Box::new(Expression::Number(12.0)),
                op: BinaryOperator::Multiply,
                right: Box::new(Expression::Number(3.0))
            }
        );
    }
```

...and so on.

We've not changed the associativity, though, so we know we're good. We should probably add another test for repeated
division (and mixed multiplication and division). First, though, let's update the `evaluate` function.

## Evaluator

In the tests, we add the following:

```rust
    #[test]
    fn division() {
        // ...
        assert_eq!(evaluate(expr), Expression::Number(4.0));
    }

    #[test]
    fn multiplication() {
        // ...
        assert_eq!(evaluate(expr), Expression::Number(36.0));
    }

    #[test]
    fn many_multiplication() {
        // ...
        assert_eq!(evaluate(expr), Expression::Number(216.0));
    }
```

And the evaluator can be fixed by adding the following:

```rust
        Expression::BinaryOperation { left, op, right } => {
            let left = evaluate(*left);
            let right = evaluate(*right);
            match (left, right) {
                (Expression::Number(left), Expression::Number(right)) => match op {
                    BinaryOperator::Divide => Expression::Number(left / right),
                    BinaryOperator::Multiply => Expression::Number(left * right),
                },
                any => panic!("{:?}", any),
            }
        }
```

And the tests all pass.

At this point, the Rust compiler is warning us that `Expression::Divide` and `Expression::Multiply` aren't used any
more, so we can get rid of those.
