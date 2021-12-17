---
title: "other_mem_fun and std::for_each"
date: 2003-02-27T14:53:00.000Z
x-drupal-nid: 56
x-needs-review: 2003-02-27T14:53:00.000Z
tags: c++
---
How many times have you found yourself writing something like the following?

    typedef std::vector<B> bs_t;

    for (bs_t::const_iterator i = bs.begin(); i != bs.end(); ++i)
    {
        do_something(*i);
    }

It gets boring quickly. Enter std::for_each:

    std::for_each(bs.begin(), bs.end(), do_something);

...which works a treat, as long as do_something() is a global function or functor; if you want to call a member function, you're SOL.

You can try this:

    std::for_each(bs.begin(), bs.end(), &B::process);

But it doesn't work. I went digging in the C++ documentation and found the mem_fun, mem_fun_ref and ptr_fun functors. They make this a little easier:

    std::for_each(bs.begin(), bs.end(), std::mem_fun(&B::process));

...should be used when you've got vector<B*>.
    std::for_each(bs.begin(), bs.end(), std::mem_fun_ref(&B::process));

...should be used when you've got vector<B>.
This is all deeply groovy. You can ignore `std::ptr_fun` -- all it seems to do is provide some typedefs when handed a function pointer. I could be wrong; it has happened before.

Now, the problem I had:

    class TestCase;

    class TestCases {
        typedef std::vector<TestCase *> testcases_t;
        testcases_t m_testcases;

        void runTest(TestCase? *testCase);

    public:
        void runTests();
    };

    void TestCases::runTests()
    {
        for (testcases_t::const_iterator i = m_testcases.begin();
             i != m_testcases.end(); ++i)
        {
            runTest(*i);
        }
    }

    void TestCases::runTest(TestCase *testCase)
    {
        MemoryLeakDetector leakDetector;
        testCase->runTest();
    }

So, I thought I'd replace the for-loop with the following:

    std::for_each(m_testcases.begin(), m_testcases.end(), mem_fun(&TestCases::runTest));

...but it doesn't work; this is geared up to call a member function *on* each of the members of the collection, rather than on something else, *with* each member.

So, enter "other_mem_fun", a concoction that I just came up with (with a non-insubstantial amount of help from Mike and Peter -- cheers guys):

    #include <functional>
    #include <algorithm>

    template <typename ReturnType, typename CalleeType, typename ArgType>
        class other_mem_fun_t
    {
        typedef ReturnType (CalleeType::*function_t) (ArgType);
        CalleeType *m_callee;
        function_t m_pfn;

    public:
        other_mem_fun_t(CalleeType *callee, function_t pfn)
            : m_callee(callee), m_pfn(pfn)
    	{
    	}

        ReturnType operator() (ArgType arg) const
        {
            return (m_callee->*m_pfn)(arg);
        }
    };

    template <typename ReturnType, typename CalleeType, typename ArgType>
    other_mem_fun_t<ReturnType, CalleeType, ArgType>
        other_mem_fun(CalleeType *callee, ReturnType (CalleeType::* pfn)(ArgType))
    {
        return other_mem_fun_t<ReturnType, CalleeType, ArgType>(callee, pfn);
    }

Fortunately, none of that mess makes it into the caller:

    void TestCases::runTests()
    {
        std::for_each(m_testcases.begin(), m_testcases.end(),
                      other_mem_fun(this, &TestCases::runTest));
    }

Cool.
