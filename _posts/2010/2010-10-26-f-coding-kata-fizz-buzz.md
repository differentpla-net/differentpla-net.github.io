---
title: "F# Coding Kata: Fizz Buzz"
date: 2010-10-26T10:53:55.000Z
x-drupal-nid: 265
x-needs-review: 2010-10-26T10:53:55.000Z
---
Described [here](http://codingkata.org/katas/unit/fizz-buzz).

<pre class="code"><span style="color: blue">let </span>IsDivisible n d =
    ((n % d) = <span style="color: purple">0</span>)

<span style="color: blue">let </span>FizzBuzz n =
    <span style="color: blue">match </span>n <span style="color: blue">with
    </span>| n <span style="color: blue">when </span>IsDivisible n <span style="color: purple">3 </span>&& IsDivisible n <span style="color: purple">5 </span><span style="color: blue">-> </span><span style="color: maroon">"fizzbuzz"
    </span>| n <span style="color: blue">when </span>IsDivisible n <span style="color: purple">3 </span><span style="color: blue">-> </span><span style="color: maroon">"fizz"
    </span>| n <span style="color: blue">when </span>IsDivisible n <span style="color: purple">5 </span><span style="color: blue">-> </span><span style="color: maroon">"buzz"
    </span>| _ <span style="color: blue">-> </span>n.ToString()

[<Fact>]
<span style="color: blue">let </span>FizzBuzz_WithSequence_ShouldReturnExpectedSequence() =
    <span style="color: blue">let </span>actual = List.map FizzBuzz [<span style="color: purple">4</span>; <span style="color: purple">5</span>; <span style="color: purple">6</span>; <span style="color: purple">7</span>; <span style="color: purple">8</span>; <span style="color: purple">9</span>; <span style="color: purple">10</span>; <span style="color: purple">11</span>; <span style="color: purple">12</span>; <span style="color: purple">13</span>; <span style="color: purple">14</span>; <span style="color: purple">15</span>]
    <span style="color: blue">let </span>expected = [<span style="color: maroon">"4"</span>; <span style="color: maroon">"buzz"</span>; <span style="color: maroon">"fizz"</span>; <span style="color: maroon">"7"</span>; <span style="color: maroon">"8"</span>; <span style="color: maroon">"fizz"</span>; <span style="color: maroon">"buzz"</span>; <span style="color: maroon">"11"</span>; <span style="color: maroon">"fizz"</span>; <span style="color: maroon">"13"</span>; <span style="color: maroon">"14"</span>; <span style="color: maroon">"fizzbuzz"</span>]
    Assert.Equal(expected, actual)</pre>