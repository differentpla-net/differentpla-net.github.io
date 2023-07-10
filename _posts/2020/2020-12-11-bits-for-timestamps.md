---
title: "How many bits for timestamps?"
date: 2020-12-11T13:43:07Z
---

Signed 32-bit `time_t`, which counts seconds since 1970 runs out after 2^31 seconds; i.e. in 2038.

You probably knew that, so you've been looking at 64-bit numbers, and you're probably thinking about using milliseconds
instead.

Here's a quick way to estimate how long that'll last.

Note that $2^{10} = 1024 \approx 1000$ meaning that every unit of one thousand needs 10 bits.

Given **64 bits**, take 10 bits for milliseconds, leaving **54 bits**.

There are 60 seconds in a minute, which is pretty close to 64 (i.e. 6 bits). That leaves you with **48 bits**.
Similarly, 60 minutes in an hour. That leaves you with **42 bits**.

Next up is hours. There are 24 hours in a day. The next largest power of two is 32, which takes 5 bits (0-31). That
leaves you with **37 bits**.

There are 365 and a bit days in a year. That's going to need 9 bits (0-511). That leaves you with **28 bits**.

That's about 250 million years.


```
         1         2         3         4         5         6   6
1234567890123456789012345678901234567890123456789012345678901234
............................DDDDDDDDDHHHHHMMMMMMssssssmmmmmmmmmm
YYYYYYYYYYYYYYYYYYYYYYYYYYYY....................................
```

| Units         | Count                 | Approx    | Bits      | Remaining |
| ---           | ---                   | ---       | ---:      | ---:      |
|               |                       |           |           | 64 bits   |
| Milliseconds  | 1000                  | ~1024     | 10 bits   | 54 bits   |
| Seconds       | 60                    | ~63       | 6 bits    | 48 bits   |
| Minutes       | 60                    | ~63       | 6 bits    | 42 bits   |
| Hours         | 24                    | ~32       | 5 bits    | 37 bits   |
| Days          | 365.25                | <512      | 9 bits    | 28 bits   |
| Years         | ~250My (but read on)  |           |           |           |

As it happens, we're out by about a factor of 2. The actual number is roughly 536 million. Why?

We wasted $log_2({32 \div 24}) \approx 0.4 $ bits in the hours/day conversion, plus another $log_2({512 \div 365})
\approx 0.5 $ bits in the days/year conversion. This gives us about a bit (i.e. our factor of two). We also wasted some
other fractional bits elsewhere (1000 &ne; 1024 and 60 &ne; 64). We erred on the cautious side, by using more bits than
needed, so our result is a lower limit.

But at these timescales it doesn't particularly matter (unless you're doing astrophysics, maybe).

If we did it all again, but with microseconds, we'd use another 10 bits, giving us $2^{18}$ years, which is $2^8 \times
2^{10}$ or $256 \times 10^3$, or roughly 250,000 years.

Note that we're still off by that factor of two, so it's actually about half a million years.

COBOL will probably still be around then. You won't.
