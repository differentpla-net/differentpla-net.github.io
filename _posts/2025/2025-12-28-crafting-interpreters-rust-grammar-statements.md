---
title: "Crafting Interpreters in Rust: Parsing statements"
short_title: Parsing statements
date: 2025-12-28T12:50:00Z
layout: series
series: crafting-interpreters-rust
tags: crafting-interpreters rust lox lalrpop december-adventure-2025
---

It's time to move on to Chapter 8: ["Statements and State"](https://craftinginterpreters.com/statements-and-state.html)
from the book. To do this, we'll implement parsing (and executing) some simple statements.

The book gives us this:

```
program        → statement* EOF ;

statement      → exprStmt
               | printStmt ;

exprStmt       → expression ";" ;
printStmt      → "print" expression ";" ;
```

So we can update our grammar accordingly:

```
// program : statement* EOF ;
pub Program = Statement*;

// statement : exprStmt
//           | printStmt ;
pub Statement = {
    ExprStmt,
    PrintStmt
};

// exprStmt : expression ";" ;
ExprStmt: Statement<'input> = <expr:Expression> ";" => Statement::Expr(expr);

// printStmt : "print" expression ";" ;
PrintStmt: Statement<'input> = "print" <expr:Expression> ";" => Statement::Print(expr);
```

Note: we make use of LALRPOP's `*` macro, which returns a `Program` as a `Vec<Statement>`.

Then we need to update the AST:

```rust
#[derive(Debug, PartialEq)]
pub enum Statement<'input> {
    Expr(Expression<'input>),
    Print(Expression<'input>)
}
```

And we can add some basic tests (note that I _didn't_ do the tests first this time):

```rust
use crate::{ast::{BinaryOperator, Expression, Statement}, lox};

#[test]
fn single_print_statement() {
    let parser = lox::ProgramParser::new();
    assert_eq!(
        parser.parse("print 6 * 9;").unwrap(),
        vec![Statement::Print(Expression::BinaryOperation {
            left: Box::new(Expression::Number(6.0)),
            op: BinaryOperator::Multiply,
            right: Box::new(Expression::Number(9.0))
        })]
    );
}
```

There's a similar test for `exprStmt`.

At this point, I decided that it'd be useful to be able to run a `.lox` program directly from the command line, so I
updated `main.rs`:

```rust
use rlox::ast::*;
use rlox::interpret::interpret;
use rlox::lox::ProgramParser;

fn main() {
    let args: Vec<String> = std::env::args().collect();

    let path = &args[1];
    let source = std::fs::read_to_string(path).expect("read file");
    let parser = ProgramParser::new();
    let program: Vec<Statement> = parser.parse(&source).unwrap();
    interpret(program);
}
```

The `interpret` function looks like this:

```rust
use crate::{ast::*, evaluate::evaluate};

pub fn interpret(program: Vec<Statement>) {
    for stmt in program {
        execute(stmt);
    }
}

fn execute(stmt: Statement<'_>) {
    match stmt {
        Statement::Expr(_) => todo!(),
        Statement::Print(expression) => println!("{}", evaluate(expression)),
    }
}
```

Note that I've not implemented `execute` for `Statement::Expr` yet. There are no possible side-effects, so it didn't
seem necessary. I'll get to that in the next part of Chapter 8, when we implement state (i.e. global variables).

The most interesting thing here is the `println!("{}", evaluate(expression))`. At the moment, `evaluate` returns
`Expression`. Because we use `{}`, we need to implement the `Display` trait for `Expression`.

It turns out that this is kinda annoying: the compiler wants us to implement `Display` for all possible variants of
`Expression`, but `evaluate` only returns some variants -- the others are reduced to this subset.

So it's time to introduce a `Value` type, and have `evaluate` return that instead:

```rust
pub fn evaluate(expr: Expression) -> Value {
    match expr {
        Expression::Nil => Value::Nil,
        Expression::Bool(b) => Value::Bool(b),
        Expression::Number(n) => Value::Number(n),
        Expression::String(s) => Value::String(s),
        Expression::Not(e) => {
            let e = evaluate(*e);
            match e {
                Value::Bool(b) => Value::Bool(!b),
                any => panic!("invert not implemented for {:?}", any),
            }
        }
        //... etc.
    }
    //... etc.
}
```

You can look at the [source code](https://codeberg.org/rlipscombe/rlox/src/commit/2aba81fa59e31e749d3881029c30b87764698969/src/evaluate.rs) for the full thing.

This also means making changes to a lot of the unit tests. Fortunately, it's fairly mechanical (and the compiler's got
our backs).

And now, we can execute actual `.lox` source files:

```
// hello.lox
// Traditionally, programming language tutorials start with a "Hello World!" program; this is ours.
print "Hello World!";
```

...at which point we discover that we can't parse comments:

```
cargo run ./examples/hello.lox
...

thread 'main' (16214183) panicked at src/main.rs:12:57:
called `Result::unwrap()` on an `Err` value: UnrecognizedToken { token: (0, Token(9, "/"), 1) ...
```

To fix this, I lifted a snippet from my previous attempt at this adventure:

```
grammar;

match {
    // Ignore C++-style comments
    r"//[^\n\r]*[\n\r]*" => {},
    // Ignore whitespace
    r"\s*" => {},
}
else {
    _
}
```

I don't know how this works, though, so I need to dig a bit deeper into LALRPOP's lexer and `match` directive.

But, with that:

```
cargo run ./examples/hello.lox
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 0.07s
     Running `target/debug/rlox ./examples/hello.lox`
Hello World!
```
