---
title: "Deleting the Internet Explorer Cache programmatically"
date: 2004-01-12T12:05:00.000Z
---

```c
char buffer[4096];
DWORD cb = 4096;

INTERNET_CACHE_ENTRY_INFO *p = (INTERNET_CACHE_ENTRY_INFO *)buffer;
HANDLE h = FindFirstUrlCacheEntry(NULL, p, &cb);
while (h)
{
    // Do something with it...
    printf("Deleting: %s...", p->lpszSourceUrlName);
    if (!DeleteUrlCacheEntry(p->lpszSourceUrlName))
    {
        printf("failed, 0x%x\n", GetLastError());
    }
    else
        printf("ok\n");

    cb = 4096;
    if (!FindNextUrlCacheEntry(h, (INTERNET_CACHE_ENTRY_INFO *)buffer, &cb))
        break;
}
```
