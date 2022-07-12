---
title: "Using [iid_is] gives me stack overflows. What's with that?"
date: 2001-07-10T09:44:50.000Z
tags: com
---

The problem here is that you have a piece of IDL like this:

```
[ object, uuid(...), oleautomation, pointer_default(unique) ]
interface IFoo
{
    HRESULT Bind(IID iid, [iid_is(iid), out] IUnknown **ppObject);
};
```

...and it doesn't work. When you try calling an object in the same apartment as the client, everything's fine. As soon
as you call an object in a different apartment (either STA to MTA or vice versa), you get "stack overflow" errors. Some
further investigation revealed that the `[iid_is]` wasn't being used to marshal the interface, and all that was getting
returned was the `IUnknown` interface.

So, I posted to the DCOM mailing list, and it turns out that `[iid_is]` and `[oleautomation]` aren't compatible.
Unfortunately, MIDL doesn't bother telling you this, it just drops the `[iid_is]` attribute. This leads to the stack
overflow error.

Possible solutions? Try one of these:

- Lose the `[iid_is]` parameter, and query the `IUnknown` once you've got it.
- Specify the exact interface you're expecting.
- Stop using TLB marshalling. This requires messing around with proxy-stub DLLs.
