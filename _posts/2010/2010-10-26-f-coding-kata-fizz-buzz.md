---
title: "F# Coding Kata: Fizz Buzz"
date: 2010-10-26T10:53:55.000Z
x-drupal-nid: 265
x-needs-review: 2010-10-26T10:53:55.000Z
---
Described [here](http://codingkata.org/katas/unit/fizz-buzz).

```fsharp
let IsDivisible n d =
    ((n % d) = 0)

let FizzBuzz n =
    match n with
    | n when IsDivisible n 3 && IsDivisible n 5 -> "fizzbuzz"
    | n when IsDivisible n 3 -> "fizz"
    | n when IsDivisible n 5 -> "buzz"
    | _ -> n.ToString()

let FizzBuzz_WithSequence_ShouldReturnExpectedSequence() =
    let actual = List.map FizzBuzz [4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15]
    let expected = ["4"; "buzz"; "fizz"; "7"; "8"; "fizz"; "buzz"; "11"; "fizz"; "13"; "14"; "fizzbuzz"]
    Assert.Equal(expected, actual)
```
