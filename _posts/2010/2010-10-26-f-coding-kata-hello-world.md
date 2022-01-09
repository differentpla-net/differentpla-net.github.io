---
title: "F# Coding Kata: Hello World"
date: 2010-10-26T10:34:34.000Z
---
So, I thought I might learn F# by doing some [Code Kata](http://codekata.pragprog.com/). I'll start by doing the exercises from [codekata.org](http://codingkata.org/). Unfortunately, they don't have any F# templates, which means I'll have to improvise at points.

The first one is "Hello World", which might seem simple enough. Writing a "Hello World" program is usually a quick smoke test for your environment: do you know how to edit and compile the source code? Do you have the correct compiler and libraries?

These days, however, there's more emphasis on unit testing, so the code katas are written in that style.

To get started, I installed [xUnit.NET](http://xunit.codeplex.com/) (I already have NUnit, but xUnit.NET is supposedly easier to use in F#). I also want to integrate it with ReSharper, so I grabbed the [xUnitContrib](http://xunitcontrib.codeplex.com/) stuff. Note: when installing the xUnitContrib stuff for ReSharper 5.1, you'll need to "Unblock" the files before Visual Studio will load them. Otherwise you get _"An attempt was made to load an assembly from a network location which would have caused the assembly to be sandboxed in previous versions of the .NET Framework. This release of the .NET Framework does not enable CAS policy by default, so this load may be dangerous."_ You'd be better off unblocking the ZIP file before you extract the files.

So, here's the results:

```fsharp
module Module1

let HelloWorld1 =
    "Hello World"

let HelloWorld2 x =
    sprintf "Hello %s" x

open Xunit

[<Fact>]
let HelloWorld_ShouldReturnHelloWorld() =
    Assert.Equal("Hello World", HelloWorld1)

[<Fact>]
let HelloWorld_WithFred_ShouldReturnHelloFred() =
    Assert.Equal("Hello Fred", HelloWorld2 "Fred")
```

Nothing particularly interesting there. I was surprised to find out that [F# doesn't support overloaded functions](http://stackoverflow.com/questions/2260939/f-overloading-functions), except as members.
