---
title: "Things I learnt this week: RegSetValueEx"
date: 2008-10-25T12:17:14.000Z
---
`RegSetValueEx`, when passed `REG_SZ`, needs the length to be in bytes, so don't just use `_tcslen` like this:

```c++
    // DON'T DO THIS
    TCHAR sz[] = _T("Hello World");
    RegSetValueEx(hKey, bstrValueName, 0, REG_SZ, (const BYTE *)sz, _tcslen(sz));
```

That truncates the value placed in the registry (11 bytes where it actually needed 24, including the null terminator).

----

`RegSetValueEx`, when passed `REG_SZ`, is documented as needing the length to include the NULL terminator.

It seems to cope OK without this, though. If you later ask for the size, it comes back with the NULL terminator included. To be on the safe side, always include the NULL terminator when passing the value to `RegSetValueEx`:

```c++
    TCHAR sz[] = _T("Hello World");
    DWORD cb = (_tcslen(sz) + 1) * sizeof(TCHAR);
    RegSetValueEx(hKey, bstrValueName, 0, REG_SZ, (const BYTE *)sz, cb);
```

----

`RegSetValueEx`, when passed REG_SZ, expects lpData to be TCHAR[], but does nothing to enforce this. This means that the following:

```c++
    // DON'T DO THIS
    char sz[] = "Hello World";
    RegSetValueEx(hKey, bstrValueName, 0, REG_SZ, (const BYTE *)sz, strlen(sz) + 1);
```

...will result in "效汬⁯潗汲d" being written to the registry.

It should be the following:

```c++
    TCHAR sz[] = _T("Hello World");
    RegSetValueEx(hKey, bstrValueName, 0, REG_SZ, (const BYTE *)sz, _tcslen(sz) + 1);
```

----

In fact, you'd be better off with the following:

```c++
LSTATUS RegSetStringValueW(_In_ HKEY hKey, _In_z_ const WCHAR *pwszValueName, _In_z_ const WCHAR *pwszData)
{
   DWORD cchData = wcslen(pwszData) + 1;
   DWORD cbData = cchData * sizeof(WCHAR);
   return RegSetValueExW(hKey, pwszValueName, 0, REG_SZ, pwszData, cbData);
}

LSTATUS RegSetStringValueA(_In_ HKEY hKey, _In_z_ const CHAR *pszValueName, _In_z_ const CHAR *pszData)
{
   DWORD cchData = strlen(pszData) + 1;
   DWORD cbData = cchData * sizeof(CHAR);
   return RegSetValueExA(hKey, pszValueName, 0, REG_SZ, pszData, cbData);
}

#ifdef UNICODE
 #define RegSetStringValue RegSetStringValueW
#else
 #define RegSetStringValue RegSetStringValueA
#endif
```
