---
title: "Test First: Roman Numeral Conversion"
date: 2004-01-12T12:07:00.000Z
x-drupal-nid: 45
x-needs-review: 2015-05-05
---
Question: Everyone "knows" that you should write a decimal to Roman number
converter using a table. What happens if you try it test-first?

A while ago someone on the [Extreme Programming (XP) mailing
list](http://groups.yahoo.com/group/extremeprogramming/) asked if it was
possible to write a Roman numeral conversion program using TFD (or TDD). I
tried it. Here's what happened.

    /* toroman.cpp - convert decimal to Roman numerals.
     * Roman numerals are I=1; V=5; X=10; L=50; C=100; D=500; M=1000
     */

    int main(void)
    {
        assert(toRoman(1) == "I");
        return 0;
    }

Not surprisingly, this piece of code doesn't compile. The compiler doesn't know
what assert or toRoman are. We'll add some code to fix this.

    /* toroman.cpp - convert decimal to Roman numerals.
     * Roman numerals are I=1; V=5; X=10; L=50; C=100; D=500; M=1000
     */

    #include <assert.h>
    std::string toRoman(int n);

    int main(void)
    {
        assert(toRoman(1) == "I");
        return 0;
    }

This doesn't compile either. I forgot to include the <string> header file.
Better fix that...

    #include <assert.h>
    #include <string>

This time it compiles, but fails to link. I don't actually have a function
called `toRoman()`

    std::string toRoman(int n)
    {
        return "I";
    }

This passes the first test case, despite the code being as simple as you like.
We'd better add another test case.

    int main(void)
    {
        assert(toRoman(1) == "I");
        assert(toRoman(2) == "II");

        return 0;
    }

This fails the second test case, so we'll add some code to handle this.

    int main(void)
    {
        assert(toRoman(0) == "");
        assert(toRoman(1) == "I");
        assert(toRoman(2) == "II");

        return 0;
    }

    std::string toRoman(int n)
    {
        if (n == 1)
            return "I";
        else if (n == 2)
            return "II";

        return "";
    }

This passes all the test cases. Because I added some code rather than have a
dangling `if`/`else`, I added a test case for it. Since it passes, it's time to
add another test.

    assert(toRoman(0) == "");
    assert(toRoman(1) == "I");
    assert(toRoman(2) == "II");
    assert(toRoman(3) == "III");

Unsurprisingly this fails. We'll add some code to handle it.

    std::string toRoman(int n)
    {
        if (n == 1)
            return "I";
        else if (n == 2)
            return "II";
        else if (n == 3)
            return "III";

        return "";
    }

All of the tests pass, so we'll try a little refactoring:

    std::string toRoman(int n)
    {
        std::string r;
        while (n--)
            r += "I";

        return r;
    }

To check that we've not broken anything, we run the tests again. Since they all
pass, we know that we've not broken anything. So we'll add another test case.

    assert(toRoman(4) == "IV");

This fails, so we'll add some code to handle this case.

    std::string toRoman(int n)
    {
        std::string r;

        if (n <= 3)
        {
            while (n--)
                r += "I";
        }
        else
        {
            r = "IV";
        }

        return r;
    }

Despite being the most worthless code ever, this works. We're happy with it.

    assert(toRoman(5) == "V");

This needs the following changes in order to pass:

    else if (n == 4)
    {
        r = "IV";
    }
    **else if (n == 5)
    {
        r = "V";
    }**

    return r;
}

This works for all test cases. I think I've spotted a possible refactoring that
will help me pass the next couple of test cases, so I'll add the tests now.

    assert(toRoman(6) == "VI");
    assert(toRoman(7) == "VII");
    assert(toRoman(8) == "VIII");

Not surprisingly, that doesn't work. We'll add some more code.

    std::string toRoman(int n)
    {
        std::string r;

        if (n <= 3)
        {
            while (n--)
                r += "I";
        }
        else if (n == 4)
        {
            r = "IV";
        }
        else if (n >= 5)
        {
            r = "V";
            n -= 5;
            while (n--)
                r += "I";
        }

        return r;
    }

That works. We're kinda in violation of
[OAOO](http://www.c2.com/cgi/wiki?OnceAndOnlyOnce) with the `while (n--)` loop,
but we'll leave it alone for the moment. We'll see where the code takes us.
Time to add some more tests.

    assert(toRoman(9) == "IX");
    assert(toRoman(10) == "X");

Running the program reveals (perhaps unsurprisingly) that these new tests fail.
We'll add some special-case code for now.

    std::string toRoman(int n)
    {
        std::string r;

        if (n <= 3)
        {
            while (n--)
                r += "I";
        }
        else if (n == 4)
        {
            r = "IV";
        }
        else if (n == 9)
        {
            r = "IX";
        }
        else if (n == 10)
        {
            r = "X";
        }
        else if (n >= 5)
        {
            r = "V";
            n -= 5;
            while (n--)
                r += "I";
        }

        return r;
    }

The tests pass. I can't see a good refactoring for this yet, so we'll add some
more tests.

    assert(toRoman(11) == "XI");
    assert(toRoman(12) == "XII");
    assert(toRoman(13) == "XIII");

They fail. Let's try this:

    // replace n == 10 clause
    else if (n >= 10)
    {
        r = "X";
        n -= 10;
        while (n--)
            r += "I";
    }

Yeah, that works. It's starting to show some kind of pattern. Time for some
more tests.

    assert(toRoman(14) == "XIV");
    assert(toRoman(15) == "XV");
    assert(toRoman(16) == "XVI");
    assert(toRoman(17) == "XVII");
    assert(toRoman(18) == "XVIII");
    assert(toRoman(19) == "XIX");
    assert(toRoman(20) == "XX");

The code that makes 14 work is showing a different pattern - it looks quite a
lot like the code for 4 and 9:

    else if (n == 9)
    {
        r = "IX";
    }
    // add this
    else if (n == 14)
    {
        r = "XIV";
    }
    else if (n >= 10)

15 still doesn't work. Now, these are very similar to the cases for 1,2,3 and 5,6,7. I'll code it up like this:

    else if (n == 14)
    {
        r = "XIV";
    }
    else if (n >= 15)
    {
        r = "XV";
        n -= 15;
        while (n--)
            r += "I";
    }
    else if (n >= 10)

This is getting a little ugly. Can I factor the last 3 into something?

    std::string toRoman(int n)
    {
        std::string r;

        if (n <= 3)
        {
            while (n--)
                r += "I";
        }
        else if (n == 4)
        {
            r = "IV";
        }
        else if (n == 9)
        {
            r = "IX";
        }
        else if (n == 14)
        {
            r = "XIV";
        }
        // Refactoring:
        else
        {
            while (n >= 10)
            {
                n -= 10;
                r += "X";
            }

            while (n >= 5)
            {
                n -= 5;
                r += "V";
            }

            while (n--)
                r += "I";
        }

        return r;
    }

Looks good. Code's cleaner. Still works for the same test cases. Still fails
for the others. Let's try to fix them.

    else if (n == 14)
    {
        r = "XIV";
    }
    else if (n == 19)
    {
        r = "XIX";
    }
    else
    {
        while (n >= 10)

This works fine for 19. 20 was already covered. We wouldn't have known without
the test case.

I'm starting to see some commonality in the 4,9,14 and 19 cases, and I think
that the n <=3 case is handled by the piece of code at the bottom. Since we're
taking baby steps, we'll deal with the n <=3 case first:

    std::string toRoman(int n)
    {
        std::string r;

        if (n == 4)
        {
            r = "IV";
        }
        else if (n == 9)
        {
            r = "IX";
        }
        else if (n == 14)
        {
            r = "XIV";
        }
        else if (n == 19)
        {
            r = "XIX";
        }
        else
        {
            while (n >= 10)
            {
                n -= 10;
                r += "X";
            }

            while (n >= 5)
            {
                n -= 5;
                r += "V";
            }

            while (n--)
                r += "I";
        }

        return r;
    }

That's got rid of the n<=3 code, and it still works. Looking good. Now, lets
add some more test cases. We'll come back to the 4,9,14,19 case later. Let's
see if we can get some tests in for the other roman numerals.

    assert(toRoman(30) == "XXX");
    assert(toRoman(40) == "XXXX");
    assert(toRoman(50) == "L");

This fails on 50 == L. We'll add some code to handle this.

    else if (n == 19)
    {
        r = "XIX";
    }
    else
    {
        while (n >= 50)
        {
            n -= 50;
            r += "L";
        }

        while (n >= 10)

Works fine. More test cases.

    assert(toRoman(60) == "LX");

## Refactor the test cases

Also works fine. The test cases are getting a little long-winded, though. Let's refactor them a little bit:

    #include <assert.h>
    #include <string>

    std::string toRoman(int n);

    #define TR(n, s) \
        { n, s, }

    int main(void)
    {
        struct test_case_t {
            int n;
            const char *s;
        } test_cases[] = {
            TR(0, ""),
            TR(1, "I"),
            TR(2, "II"),
            TR(3, "III"),
            TR(4, "IV"),
            TR(5, "V"),
            TR(6, "VI"),
            TR(7, "VII"),
            TR(8, "VIII"),
            TR(9, "IX"),
            TR(10, "X"),
            TR(11, "XI"),
            TR(12, "XII"),
            TR(13, "XIII"),
            TR(14, "XIV"),
            TR(15, "XV"),
            TR(16, "XVI"),
            TR(17, "XVII"),
            TR(18, "XVIII"),
            TR(19, "XIX"),
            TR(20, "XX"),
            TR(30, "XXX"),
            TR(40, "XXXX"),
            TR(50, "L"),
            TR(60, "LXI"),
        };

        for (int q = 0; q < sizeof(test_cases) / sizeof(test_cases[0]); ++q)
        {
            int n = test_cases[q].n;
            std::string got = toRoman(n);
            std::string expected(test_cases[q].s);

            if (got != expected)
            {
                fprintf(stderr, "n = %d, expected '%s', got '%s'\n", n,
                        expected.c_str(), got.c_str());
            }
        }

        return 0;
    }

Notice that we've deliberately broken one of the test cases. This is to see
whether the test case is actually getting run. We'll put it back, and add some
more. We also note that the correct result for 40 is actually XL, not XXXX, so
we fix the test case.

    TR(40, "XL"),
    TR(50, "L"),
    TR(60, "LX"),
    TR(70, "LXX"),
    TR(80, "LXXX"),
    TR(90, "XC"),
    TR(100, "C"),

Now 40,90,100 fail. 40 and 90 look very similar to the 4 and 9 cases. We'll
special-case them for now. On the other hand, 100 looks a lot like the 10 and
50 cases, so we'll add some code for that.

    else if (n == 19)
    {
        r = "XIX";
    }
    else if (n == 40)
    {
        r = "XL";
    }
    else if (n == 90)
    {
        r = "XC";
    }
    else
    {
        while (n >= 100)
        {
            n -= 100;
            r += "C";
        }

        while (n >= 50)

I'm now thinking that I could reuse the bottom of that function by bumping the
values up by what's missing, and falling through. For example, we'd spot the 4,
put I in the result, and then add one, allowing the following code to put the
correct V on the result. We'll see. For now, more test cases.

    TR(200, "CC"),
    TR(300, "CCC"),
    TR(400, "CD"),
    TR(500, "D"),
    TR(600, "DC"),
    TR(700, "DCC"),
    TR(800, "DCCC"),
    TR(900, "CM"),
    TR(1000, "M"),

Not surprisingly, these fail. More code.

    else if (n == 90)
    {
        r = "XC";
    }
    else if (n == 400)
    {
        r = "CD";
    }
    else if (n == 900)
    {
        r = "CM";
    }
    else
    {
        while (n >= 500)
        {
            n -= 500;
            r += "D";
        }

        while (n >= 100)

That gets us up to 900. I suspect that M will be quite simple.

    else
    {
        while (n >= 1000)
        {
            n -= 1000;
            r += "M";
        }

        while (n >= 500)

That gets all of our tests to pass. We'll add some more test cases, to see where that takes us.

    TR(21, "XXI"),
    TR(29, "XXIX"),
    TR(34, "XXXIV"),

21 succeeds. 29 and 34 don't. This is down to the special-casing.

Let's see if we can roll it up into a loop. After a quick bit of refactoring
and debugging, we end up with the following:

    std::string toRoman(int n)
    {
        std::string r;

        while (n)
        {
            while (n >= 1000)
            {
                n -= 1000;
                r += "M";
            }

            if (n == 900)
            {
                r += "CM";
                n -= 900;
            }

            while (n >= 500)
            {
                n -= 500;
                r += "D";
            }

            if (n == 400)
            {
                r += "CD";
                n -= 400;
            }

            while (n >= 100)
            {
                n -= 100;
                r += "C";
            }

            if (n == 90)
            {
                r += "XC";
                n -= 90;
            }

            while (n >= 50)
            {
                n -= 50;
                r += "L";
            }

            if (n == 40)
            {
                r += "XL";
                n -= 40;
            }

            while (n >= 10)
            {
                n -= 10;
                r += "X";
            }

            if (n == 9)
            {
                r += "IX";
                n -= 9;
            }

            while (n >= 5)
            {
                n -= 5;
                r += "V";
            }

            if (n == 4)
            {
                r += "IV";
                n -= 4;
            }

            while (n >= 1)
            {
                n -= 1;
                r += "I";
            }
        }

        return r;
    }

This works fine for our existing tests. I was going say something along the
lines of "and that's easy to turn into a table-driven solution", and call it a
day. Unfortunately, it doesn't actually work. I added some more esoteric
numbers:

    TR(1900, "MCM"),
    TR(1975, "MCMLXXV"),
    TR(1989, "MCMLXXXIX"),
    TR(1999, "MCMXCIX"),
    TR(2000, "MM"),
    TR(2001, "MMI"),

Some of the tests passed. Some didn't. Stepping through with the debugger
revealed that the equality checks in the 4,9,etc. cases were to blame. So I
changed them all to >=. This made all of the tests pass.

## Table-driven solution

It also makes it more obvious how to turn `toRoman` into a table-driven
implementation:

    std::string toRoman(int n)
    {
        std::string r;

        struct TO_ROMAN {
            int num;
            const char *str;
        } to_roman[] = {
            { 1000, "M", },
            { 900, "CM", },
            { 500, "D", },
            { 400, "CD", },
            { 100, "C", },
            { 90, "XC", },
            { 50, "L", },
            { 40, "XL", },
            { 10, "X", },
            { 9, "IX", },
            { 5, "V", },
            { 4, "IV", },
            { 1, "I", },
        };

        for (int q = 0; q < sizeof(to_roman) / sizeof(to_roman[0]); ++q)
        {
            TO_ROMAN *t = &to_roman[q];

            while (n >= t->num)
            {
                n -= t->num;
                r += t->str;
            }
        }

        return r;
    }

And I know this works, because I've got the tests to prove it.

*Note that there were some comments here originally; they got lost in the
website migration*
