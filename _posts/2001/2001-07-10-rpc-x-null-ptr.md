---
title: "RPC_X_NULL_REF_POINTER - what?"
date: 2001-07-10T09:44:57.000Z
tags: com
---

I just came across a problem where `IEnumFoo::Next` was returning`RPC_X_NULL_REF_POINTER`. Of course, the first thing that I did wasundertake a search in MSDN, and with Google to see if I could find anyexplanation of what the problem is.

Of course, all I managed to find was about 60 copies of what `winerror.h` says. Some of them were translated into different languages (human and computer). Not a lot of use.

`winerror.h` simply says &quot;A null reference pointer was passed to the stub.&quot;

What it means, however, is that you didn't pay any attention to the parameters of `IEnumFoo::Next()`. `Next` is prototyped like this:

```c++
HRESULT Next (
    [in] ULONG celt,
    [out, size_is(celt)] Foo rgelt[],
    [out] ULONG *pceltFetched);
```

The documentation for `IEnumFoo::Next` states that the last parameter (`pceltFetched`) can be NULL, if and only if
`celt` is **equal to one**.

The difficulty here comes from trying to marshal this NULL pointer across an apartment or process boundary. It's not
allowed.

The standard Enum interfaces, e.g. `IEnumVARIANT`, have to mess around with `[local]` and `[call_as]` IDL attributes in
order to implement this get-out clause given people by the documentation.

The moral of the story: If you're going to strictly adhere to the letter of the law, you should do the same. If, on the
other hand, you're feeling lazy (like me), just pass something in `pceltFetched`.
