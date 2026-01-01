---
title: "Crafting Interpreters in Rust: Parsing variable declarations"
short_title: Parsing variable declarations
date: 2026-01-01T17:13:00Z
layout: series
series: crafting-interpreters-rust
tags: crafting-interpreters rust lox lalrpop december-adventure-2025
---

In the previous post, we started on Chapter 8: ["Statements and
State"](https://craftinginterpreters.com/statements-and-state.html) from the book. We implemented some simple
statements, primarily `print`.

In this post, we'll look at the next type of statement: global variable declarations.

The book tells us that variables are declared with the `var` keyword:

```
var beverage = "espresso";
```

...and that they're different from normal statements, which gives us this grammar:

```
program        → declaration* EOF ;

declaration    → varDecl
               | statement ;
```

So we update our grammar to match:

```
// program : declaration* EOF ;
pub Program = Declaration*;

// declaration : varDecl
//             | statement ;
Declaration = {
    VarDecl,
    Statement
};
```

## VarDecl

The book gives us the following EBNF for `varDecl`:

```
// varDecl : "var" IDENTIFIER ( "=" expression )? ";" ;
```

Before we get into discussing how to implement this, we'll get `IDENTIFIER` out of the way first:

```
// An identifier is an upper- or lower-case letter (or underscore) followed
// by zero or more upper- or lower-case letters, numbers or underscores.
Identifier = r"[a-zA-Z_][a-zA-Z0-9_]*";
```

## Using the `?` macro to parse VarDecl

There are a few options for `VarDecl`. 

If we convert the given EBNF directly to LALRPOP, using the `?` macro, we get the following valid LALRPOP grammar:

```
// varDecl : "var" IDENTIFIER ( "=" expression )? ";" ;
VarDecl = "var" Identifier ( "=" Expression )? ";";
```

But that fails because `Declaration`, above, needs both alternatives to be of type `Statement`. We can fix that with a
type declaration:

```
VarDecl: Statement<'input> = "var" Identifier ( "=" Expression )? ";";
```

But now the compiler fails:

```
expected `Statement<'_>`, found `(&str, &str, Option<(&str, Expression<'_>)>, &str)`
```

Our grammar rule returns a tuple that looks like this: `("var", identifier, Option( ("=", expression) ), ";")`, and
that's not a `Statement`.

The LALRPOP tutorial chapter on [macros](https://lalrpop.github.io/lalrpop/tutorial/006_macros.html) suggests that we
can do something like this:

```
VarDecl: Statement<'input> = "var" Identifier ( "=" Expression )? ";" => match(<>) {
    // ???
};
```

Now the compiler complains that we've not filled out all of the match arms. We can do that:

```
VarDecl: Statement<'input> = "var" Identifier ( "=" Expression )? ";" => match(<>) {
    (&_, id, Some((&_, expr)), &_) => Statement::Var(id, expr),
    (&_, id, None, &_) => Statement::Var(id, Expression::Nil),
};
```

...and now it builds. Kinda ugly, though. We can do better:

```
VarDecl: Statement<'input> = "var" <Identifier> <( "=" <Expression> )?> ";";
```

By wrapping the optional part in angle brackets, we tell LALRPOP that we want the value of the contained `Expression`,
not a tuple of the equals and the expression. However, if we do that, we also need to wrap `Identifier` and `Expression`
in angle brackets, to tell LALRPOP to capture them.

Now the compiler fails with something closer to what we want:

```
expected `Statement<'_>`, found `(&str, Option<Expression<'_>>)`
```

We'll need to add a custom action, but first I want to name the matches:

```
VarDecl: Statement<'input> = "var" <id:Identifier> <expr:( "=" <Expression> )?> ";";
```

Note that the `expr` name applies to the `?` macro, not to the contained expression.

Then we can implement the custom action:

```
VarDecl: Statement<'input> = "var" <id:Identifier> <expr:( "=" <Expression> )?> ";" => match (id, expr) {
    (id, None) => Statement::Var(id, Expression::Nil),
    (id, Some(expr)) => Statement::Var(id, expr)
};
```

If you don't like how that looks, it can be tidied up a little bit:

```
VarDecl: Statement<'input> = {
    "var" <id:Identifier> <expr:( "=" <Expression> )?> ";" => {
        match (id, expr) {
            (id, None) => Statement::Var(id, Expression::Nil),
            (id, Some(expr)) => Statement::Var(id, expr)
        }
    }
};
```

## Alternatively

Or, if you prefer, you could write it as follows:

```
VarDecl: Statement<'input> = {
    "var" <id:Identifier> ";" => Statement::Var(id, Expression::Nil),
    "var" <id:Identifier> "=" <expr:Expression> ";" => Statement::Var(id, expr)
};
```

Honestly, I don't know which one I prefer.

## Tests

We can add some tests:

```rust
#[test]
fn global_var_declaration() {
    let parser = lox::ProgramParser::new();
    assert_eq!(
        parser.parse("var x;").unwrap(),
        vec![Statement::Var("x", Expression::Nil)]
    );
}

#[test]
fn global_var_declaration_constant() {
    let parser = lox::ProgramParser::new();
    assert_eq!(
        parser.parse("var y = false;").unwrap(),
        vec![Statement::Var("y", Expression::Bool(false))]
    );
}

#[test]
fn global_var_declaration_expression() {
    let parser = lox::ProgramParser::new();
    assert_eq!(
        parser.parse("var z = 12 * 34;").unwrap(),
        vec![Statement::Var(
            "z",
            Expression::BinaryOperation {
                left: Box::new(Expression::Number(12.0)),
                op: BinaryOperator::Multiply,
                right: Box::new(Expression::Number(34.0))
            }
        )]
    );
}
```

## Interpreter

We also need a placeholder in the interpreter:

```rust
    // ...
    match stmt {
        Statement::Expr(_) => todo!(),
        Statement::Print(expression) => println!("{}", evaluate(expression)),
        
        Statement::Var(_, _expression) => todo!(),
    }
```