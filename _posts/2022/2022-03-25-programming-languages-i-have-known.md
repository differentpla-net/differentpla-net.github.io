---
title: "Programming Languages I Have Known"
date: 2022-03-25T16:13:00Z
---

I'm learning Dart and it got me wondering: "Just how many programming languages have you used, Roger?".

The thing about languages -- programming or otherwise -- is that you reach a level of ability: are you a beginner, are
you proficient, are you fluent? And, after that, if you don't use a language (or any other skill) for a while, it
atrophies.

Depending on exactly how you count, it's something like 40 languages. Here's the breakdown.

## Current, Familiar

These are the languages I'm currently familiar with:

- [Erlang](https://www.erlang.org/).
  We [wrote the Electric Imp server-side components in Erlang]({% post_url 2014/2014-05-16-electricimp-erlang %}), mostly.
- [Elixir](https://elixir-lang.org/). But some of the newer stuff uses Elixir.
- [C++](https://en.wikipedia.org/wiki/C%2B%2B), but only up to C++11 or so. Yes, it's 2022, so how can I be current in a
  language version from a decade ago? The thing is: when you're writing for an embedded platform, or you're maintaining
  a legacy code base, there doesn't tend to be much call for using newer language features.
- [Bash scripting](https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html).
- [jq](https://stedolan.github.io/jq/). Yes, it's a programming language.
- [AWK](https://www.gnu.org/software/gawk/manual/gawk.html).
- [Sed](https://www.gnu.org/software/sed/manual/sed.html).

## Current, Intermediate

The list of languages that I use daily, or have used recently, but where I've not gone into much depth:

- [Python](https://www.python.org/). I'm not a huge fan, but it has its place.
- [HTML](https://developer.mozilla.org/en-US/docs/Learn/Getting_started_with_the_web/HTML_basics). Gotta be able to put
  a basic website together, right? Even if it's just templates for a static site generator.
- [Squirrel](http://squirrel-lang.org/). The sandboxed language used by [Electric Imp's
  platform](https://www.electricimp.com/). I probably know more about how the VM is implemented than I do about the
  actual language.
- [JavaScript](https://en.wikipedia.org/wiki/JavaScript). If I _had_ to do Javascript development, I'd use
  [TypeScript](https://www.typescriptlang.org/) or a transpiler these days, I think. [Fable](https://fable.io/) looks
  cool.

## Current, Learning

Then there's the list of languages that I've been playing around with recently, to learn something new:

- [Rust](https://www.rust-lang.org/). Because it's not C++.
- [WASM](https://webassembly.org/). Would it be possible to compile Squirrel to WASM? Dunno, but I played with WASM
  once, and plan on taking another look.
- [Dart](https://dart.dev/). I've got an Android side project, so I'm playing with Dart/Flutter.
- [Kotlin](https://kotlinlang.org/). That Android side project, again, and I'm not a big fan of Java.
- [F#](https://fsharp.org/). Played with it a bit for Advent of Code.

## Current, Beginner

The list of languages that I've used recently, to get something done:

- [Lua](https://www.lua.org/). We used it for [nginx](https://www.nginx.com/) extensions at Electric Imp.
- [Perl](https://www.perl.org/). Text processing of log files.
- Tcl/Expect. Some regression test scripting.
- CSS. Even static websites need a bit of styling occasionally.
- Groovy. Jenkins pipelines, mostly.
- XSLT. I had an XML export that needed transforming.

## Former, Familiar

- C#. I used C# for many, many years. They've added a bunch of new features, but I reckon I could pick it back up fairly
  quickly.
- SQL. Mostly T-SQL, but I started with Oracle PL/SQL and can do MySQL and PostgreSQL.
- PowerShell. See [PSBouncyCastle](https://github.com/rlipscombe/PSBouncyCastle) for example. We also wrote installers
  in it before Octopus Deploy was a thing.
- NAnt. I once wrote an entire CI system in it. Including the polling, scheduling and emailing parts. No, really.

## Former, Intermediate

- [Ruby](https://www.ruby-lang.org/en/).
  Learnt it with the [Pickaxe book](https://pragprog.com/titles/ruby/programming-ruby-2nd-edition/).
  Still occasionally use [Rake](https://ruby.github.io/rake/) for some things.
  Haven't bothered with [Rails](https://rubyonrails.org/).
- [PHP](https://www.php.net/). I once ran my website with [Drupal](https://www.drupal.org/). I dug into the PHP code
  occasionally. Don't fancy going back.
- JScript. Before PowerShell.
- VBScript. Before PowerShell.
- Batch. Before PowerShell. I guess you could get fluent with Batch files, but you should probably seek help at that
  point.
- MSBuild. Enough to be dangerous.

## Former, Beginner

- Assembly (ARM). I learnt how to read it at [empeg](https://www.empeg.com/), but never got that proficient.
- [CoffeeScript](https://coffeescript.org/). I wrote some extensions to [Dashing](http://dashing.io/) at one point.
- Pro*C. Blast from the past. Embedded SQL statements in C code, for Oracle. Used it once at my second job after
  graduating.

## Ancient, Familiar

These are languages that I used a _lot_, but haven't used in _years_:

- C. I've still got a copy of K&R 2e on my bookshelf. I started with MixC, then QuickC for DOS. I programmed for Windows
  in C, since (draws breath) about 1990.
- Assembly (x86). Because back in those days, if you wanted small and fast, you did it in x86 assembler (MASM, as it
  happens).
- QuickBASIC. I once wrote a multi-windowed chat app using a mix of QuickBASIC, QuickC and MASM.
- RM BASIC. I used this at school until QuickBASIC became the new hotness.

## Ancient, Beginner

I used these at university and never again since:

- Assembly (68000).
- Assembly (32016).
- Pascal.
- Miranda. ML-influenced functional programming language. It was proprietary at the time. It's open-source these days. I
  guess I'd use F# or OCaml instead, though.
- Either Eiffel or Modula-2. I don't remember which.
- Z. We once did a group project to formally prove ... something.
- Prolog.

I used these at school. For the sake of the USians, let's say it was "High School":

- BBC BASIC.
- Logo.

## Appendix: Would Like to Learn

There's also a list of languages that I should probably learn, but haven't got around to it yet:

- Racket. Everyone should learn LISP, right?
- OCaml. Everyone should learn an ML-family language, right?
- [Haskell](https://www.haskell.org/). Everyone should learn Haskell, right?
- Go. I'm not a huge fan, but if I'm going to get much deeper with Kubernetes, it seems like I'm going to have to learn
  it at some point. Maybe I'll wait until generics are a bit more widespread.
- Gleam. Type safe language for BEAM.
- Caramel. ML-family language for BEAM. I like the BEAM.
