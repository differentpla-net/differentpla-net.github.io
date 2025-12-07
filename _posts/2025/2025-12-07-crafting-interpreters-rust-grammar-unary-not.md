---
title: "Crafting Interpreters in Rust: Parsing Unary-NOT"
short_title: Parsing unary-NOT
date: 2025-12-07T12:00:00Z
layout: series
series: crafting-interpreters-rust
tags: crafting-interpreters rust lox lalrpop december-adventure
---

We're implementing Lox's `primary` rule. So far, we've done booleans, `nil`, numbers and strings. Let's move on to some
basic expressions. In this post, we'll deal with unary-NOT -- `!x`.

In the book, Lox's [`primary` rule](https://craftinginterpreters.com/parsing-expressions.html) looks like this:

```
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
```

We've already done numbers, strings, boleans and nil. Now we need to do expressions, which are defined later in the chapter like this:

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

That is: the whole grammar is recursive. So, to make life easier, we'll work from the bottom up, and we'll implement the following instead:

```
expression     → unary ;
unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" expression ")" ;
```

Then we can add the lower-precedence rules one at a time.

Let's add some basic tests for unary-NOT:

```rust
    #[test]
    fn not() {
        let parser = lox::ExpressionParser::new();
        assert_eq!(parser.parse("!false").unwrap(), Primary::Bool(true));
        assert_eq!(parser.parse("!true").unwrap(), Primary::Bool(false));
    }
```

We're using `ExpressionParser`, rather than `PrimaryParser`, which doesn't exist, so we temporarily disable the new test
and update one of the old ones to force us to implement that first:

```rust
    #[test]
    fn true_() {
        let parser = lox::ExpressionParser::new();
        assert_eq!(parser.parse("true").unwrap(), Primary::Bool(true));
    }
```

Then we update the grammar to add `Expression`.

```
pub Expression = Primary;
```

That's kinda cheating: we ought to return an `Expression` type. We can fix that by renaming the `Primary` type to
`Expression`. The grammar needs all of the extra "types" to enforce precedence, but the AST itself doesn't -- as long as
we correctly implement associativity later.

```rust
// ast.rs
#[derive(Debug, PartialEq)]
pub enum Expression<'input> {
    Nil,
    Bool(bool),
    Number(f64),
    String(&'input str)
}
```

Then we search-and-replace `Primary` to `Expression` (and `PrimaryParser` to `ExpressionParser`) in the rest of the
code, and all of the existing tests pass.

_Now_, finally, we can re-enable the failing unary-NOT test:

```rust
    #[test]
    fn not() {
        let parser = lox::ExpressionParser::new();
        assert_eq!(parser.parse("!false").unwrap(), Expression::Bool(true));
        assert_eq!(parser.parse("!true").unwrap(), Expression::Bool(false));
    }
```

...and we can add the operator to the grammar:

```
pub Expression = Unary;

Unary = {
    "!" <expr: Unary> => Expression::Not(Box::new(expr)),
    Primary
};
```

We need to add that to the AST as well:

```rust
// ast.rs
#[derive(Debug, PartialEq)]
pub enum Expression<'input> {
    Nil,
    Bool(bool),
    Number(f64),
    String(&'input str),
    Not(Box<Expression<'input>>)
}
```

Note that, because the type is recursive, we need to introduce `Box`.

Having done all of that, the code compiles, but the new test fails, because we're expecting `Bool(true)`, but we're
actually getting `Not(Bool(false))`.

We can fix the test fairly simply, but it also raises the question: "Why don't we simply evaluate the term while
parsing?". We could parse `!false` and turn it into `Bool(true)` directly. To which the answer is: "yes, we could, but
what if -- later -- we're parsing `!var` or `!func()`?". So we leave that stuff out of the parser and deal with it
later.

Let's fix the test:

```
    #[test]
    fn not() {
        let parser = lox::ExpressionParser::new();
        assert_eq!(parser.parse("!false").unwrap(), Expression::Not(Box::new(Expression::Bool(false))));
        assert_eq!(parser.parse("!true").unwrap(), Expression::Not(Box::new(Expression::Bool(true))));
    }
```

For the lulz, we can put together a really simple expression evaluator that deals with this. Here's the test:

```rust
    #[test]
    fn eval_not_false() {
        let parser = lox::ExpressionParser::new();
        let expr = parser.parse("!false").unwrap();
        assert_eq!(evaluate(expr), Expression::Bool(true));
    }
```

Here's the implementation:

```rust
fn evaluate(_expr: ast::Expression) -> ast::Expression {
    ast::Expression::Bool(true)
}
```

Oh, yeah: cheating. To force an actually-correct implementation, we need to add another assertion:

```rust
    #[test]
    fn eval_not_true() {
        let parser = lox::ExpressionParser::new();
        let expr = parser.parse("!true").unwrap();
        assert_eq!(evaluate(expr), Expression::Bool(false));
    }
```

That forces us to update `evaluate` so that it looks like this:

```rust
fn evaluate(expr: ast::Expression) -> ast::Expression {
    match expr {
        ast::Expression::Not(v) => {
            match *v {
                ast::Expression::Bool(b) => ast::Expression::Bool(!b),
                _ => panic!()
            }
        },
        _ => panic!()
    }
}
```

No, it's not pretty, but the tests pass. Let's add another test:

```rust
    #[test]
    fn eval_not_not_true() {
        let parser = lox::ExpressionParser::new();
        let expr = parser.parse("!!true").unwrap();
        assert_eq!(evaluate(expr), Expression::Bool(true));
    }
```

That forces us to update `evaluate` to look like this:

```rust
pub fn evaluate(expr: ast::Expression) -> ast::Expression {
    match expr {
        ast::Expression::Not(v) => {
            let v = evaluate(*v);
            match v {
                ast::Expression::Bool(b) => ast::Expression::Bool(!b),
                any => panic!("{:?}", any)
            }
        },
        any => any
    }
}
```

And all of the tests pass. Let's leave it there for now. We'll worry about the semantics of unary-NOT with non-booleans
later. In the next post, we'll take a look at adding the other unary operator: negation.
