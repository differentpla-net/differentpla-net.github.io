---
title: "Enumerating interfaces supported by a COM object"
date: 2001-10-03T15:58:50.000Z
tags: com
---

Something like this:

```c++
LONG EnumerateInterfacesOnObject(IUnknown *pUnk)
{
    HKEY k;
    LONG result = RegOpenKeyEx(HKEY_CLASSES_ROOT, "Interface", 0, KEY_READ, &k);
    if (result != NOERROR) return result;

    DWORD cSubKeys;
    DWORD cbMaxSubKeyLen;
    result = RegQueryInfoKey(k, NULL, NULL, NULL, &cSubKeys, &cbMaxSubKeyLen,
                             NULL, NULL, NULL, NULL, NULL, NULL);
    if (result != NOERROR) return result;

    DWORD cbName = cbMaxSubKeyLen + 1;
    TCHAR *pName = NEW TCHAR[cbName];
    DWORD cbDefault = 255;
    TCHAR *pDefault = NEW TCHAR[cbDefault];

    for (DWORD index = 0; index &lt; cSubKeys; ++index)
    {
        USES_CONVERSION;

        cbName = cbMaxSubKeyLen + 1;
        RegEnumKeyEx(k, index, pName, &cbName, NULL, NULL, NULL, NULL);

        // Convert that into an IID
        IID iid;
        if (FAILED(CLSIDFromString(T2OLE(pName), &iid)))
            continue;

        // Open that key, to look up the name
        HKEY k2;
        result = RegOpenKeyEx(k, pName, 0, KEY_READ, &k2);
        if (result != NOERROR) continue;

        cbDefault = 255;
        RegQueryValueEx(k2, "", NULL, NULL, (BYTE *)pDefault, &cbDefault);
        RegCloseKey(k2);

        // Query our object for it.
        IUnknown *pUnused;
        if (SUCCEEDED(pUnk->QueryInterface(iid, (void **)&pUnused)))
        {
            printf("  {IID_%s}\n", pDefault);
            pUnused->Release();
        }
    }

    delete [] pName;
    RegCloseKey(k);
}
```
