---
title: "Bit for timestamps"
date: 2020-12-11T13:43:07Z
---

Signed 32-bit `time_t`, which counts seconds since 1970 runs out after 2^31 seconds;
i.e. in 2038.

You probably knew that, so you've been looking at 64-bit numbers, and you're probably
thinking about using milliseconds instead.

Here's a quick way to estimate how long that'll last.

Note that 2^10 (1024) ~= 10^3 (1000) meaning that every unit of one thousand
needs 10 bits.

Given **64 bits**, take 10 bits for milliseconds, leaving **54 bits**.

There are 60 seconds in a minute, which is pretty close to 63 (i.e. 6 bits). That leaves you with **48 bits**. Similarly, 60 minutes in an hour. That leaves you with **42 bits**.

Next up is hours. There are 24 hours in a day. The next largest power of two is 32, which takes 5 bits (0-31). That leaves you with **37 bits**.

There are 365 and a bit days in a year. That's going to need 9 bits (0-511). That leaves you with **28 bits**.

That's about 250 million years.


```
         1         2         3         4         5         6   6
1234567890123456789012345678901234567890123456789012345678901234
............................DDDDDDDDDHHHHHMMMMMMssssssmmmmmmmmmm
```

<table border="1">
<tr><th>Units</th><th>&nbsp;</th><th></th><th>Bits</th><th>Remaining</th></tr>
<tr><td></td><td></td><td></td><td></td><td>64 bits</td></tr>
<tr><td>Milliseconds</td><td>1000</td><td>~1023</td><td>10 bits</td><td>54 bits</td></tr>
<tr><td>Seconds</td><td>60</td><td>~63</td><td>6 bits</td><td>48 bits</td></tr>
<tr><td>Minutes</td><td>60</td><td>~63</td><td>6 bits</td><td>42 bits</td></tr>
<tr><td>Hours</td><td>24</td><td>~31</td><td>5 bits</td><td>37 bits</td></tr>
<tr><td>Days</td><td>365.25</td><td>~511</td><td>9 bits</td><td>28 bits</td></tr>
<tr><td>Years</td><td>~250My (but read on)</td><td></td><td>28 bits</td><td></td></tr>
</table>

As it happens, we're out by about a factor of 2. The actual number is roughly 536 million. Why?

We wasted ~0.4 bits in the hours/day conversion, plus another ~0.5 bits in the days/year conversion. This gives us about a bit (i.e. our factor of two). We also wasted some other fractional bits elsewhere (1000 &#x2260; 1023 and 60 &#x2260; 63). We erred on the cautious side, by using more bits than needed, so our result is a lower limit.

But at these timescales it doesn't particularly matter (unless you're doing astrophysics, maybe).

If we did it all again, but with microseconds, we'd use another 10 bits, giving us 2^18 years, which is (2^10 * 2^8) or (~10^3 * 256), or roughly 250,000 years.

Note that we're still off by that factor of two, so it's actually about half a million years.

COBOL will probably still be around then. You won't.
