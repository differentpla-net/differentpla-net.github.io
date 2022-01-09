---
title: "Things I learnt this week: RegQueryValueEx"
date: 2008-10-25T12:21:49.000Z
---
RegQueryValueEx, when passed lpData = NULL, will set *lpcbData to the length required in bytes, even if it's already set to something. If lpData != NULL, and *lpcbData is too short, RegQueryValueEx will return ERROR_MORE_DATA.

This means that you probably ought to call it in a loop, like this:

<pre>   DWORD cbData = 0;
   BYTE *pData = (BYTE *)malloc(cbData);
   do
   {
      result = RegQueryValueEx(hKey, pszValueName, NULL, NULL, pData, &cbData);
      if (result == ERROR_SUCCESS)
         break;

      free(pData);
      pData = (BYTE *)malloc(cbData);
   } while (result == ERROR_MORE_DATA);

   if (result == ERROR_SUCCESS)
   {
      // Do something with pData
   }

   free(pData);</pre>
