---
title: "Using FindWindow in an MFC application"
date: 2003-08-05T12:46:22.000Z
tags: mfc
---

There are at least two reasons for using `FindWindow`.

- To activate the previous instance of your application.
- To close your application while the installer is upgrading it.

## Changing window class names in MFC

Unfortunately, `FindWindow` is only really guaranteed to return your application's main window if you use a unique class
name.  If you're using MFC, you'll find this a little difficult: MFC automatically generates class names of the form
"Afx:400000:8:10011:0:7e0c0b".

To use your own class name, you'll need to override `PreCreateWindow`:

```c++
BOOL CMainFrame::PreCreateWindow(CREATESTRUCT& cs)
{
    if(!CMDIFrameWnd::PreCreateWindow(cs))
        return FALSE;

    // When called from LoadFrame, via GetIconWndClass, we're supposed
    // to return the default window class name (something like
    // "AfxMDIFrame42").
    //
    // This information is then modified and used to register the
    // "Afx:x:x:x:" class, using the relevant icon/cursor/etc.

    // The new class name is then passed to CreateEx.  We're called
    // again to modify styles, etc.

    // Now, we _could_ fill in the WNDCLASS structure and register the
    // class fully the first time.

    // Or, we could be lazy and let MFC fill it in for us, and then
    // just register it with a different name.  If we do this, we need
    // to be careful not to change the default class name during
    // GetIconWndClass.

    // The first time through, cs.hInstance is NULL.
    if (cs.hInstance)
    {
        static const TCHAR className[] = _T("UniqueWindowClassName");

        // Get the existing WNDCLASS.
        WNDCLASS wc;
        VERIFY(GetClassInfo(cs.hInstance, cs.lpszClass, &wc));

        // Change the name.
        wc.lpszClassName = className;

        // Register the renamed WNDCLASS.
        VERIFY(AfxRegisterClass(&wc));

        // Use the new one.
        cs.lpszClass = className;
    }

    return TRUE;
}
```

## Activating the previous instance of your application

Put this code near the top of your `InitInstance` function:

```c++
    const TCHAR semaphoreName[] = _T("UniqueSemaphoreName");
    HANDLE h = CreateSemaphore(NULL, 0, 1, semaphoreName);
    if (GetLastError() == ERROR_ALREADY_EXISTS)
    {
        // Then we're already running.  Find and activate the previous instance.
        static const TCHAR className[] = _T("UniqueWindowClassName");

        HWND hwndPrevious = ::FindWindow(className, NULL);
        if (hwndPrevious)
            SetForegroundWindow(hwndPrevious);

        return FALSE;        // Bail; don't run message loop.
    }
```

## Closing the previous instance of your application

Similar to the above:

```c++
    const TCHAR semaphoreName[] = _T("UniqueSemaphoreName");
    HANDLE h = CreateSemaphore(NULL, 0, 1, semaphoreName);
    if (GetLastError() == ERROR_ALREADY_EXISTS)
    {
        // Then we're already running.  Find and activate the previous instance.
        static const TCHAR className[] = _T("UniqueWindowClassName");

        HWND hwndPrevious = ::FindWindow(className, NULL);
        if (hwndPrevious)
            ::SendMessage(hwndPrevious, WM_CLOSE, 0, 0);

        return FALSE;        // Bail; don't run message loop.
    }
```
