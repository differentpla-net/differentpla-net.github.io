---
title: "Many to One Dependencies"
date: 2003-04-17T09:30:46.000Z
tags: mfc
---

## Using a modal dialog as a main window

If you ask the MFC AppWizard to generate a dialog-based application, it generates code that looks like this:

```c++
BOOL CModalApp::InitInstance()
{
    CModalDlg dlg;
    m_pMainWnd = &dlg;
    int nResponse = dlg.DoModal();

    // TODO: Do something, based on nResponse (IDOK or IDCANCEL)

    // Since the dialog has been closed, return FALSE so that we exit the
    // application, rather than start the application's message pump.
    return FALSE;
}
```

## Using a modeless dialog as a main window

Unfortunately, this doesn't work properly if you have control bars in your dialog.  This is because `DoModal` doesn't
use the standard MFC message loop.  This means that `OnIdle` is not called.  If `OnIdle` is not called, then
`WM_IDLEUPDATECMDUI` is not sent, and the control bars aren't updated correctly.

We solve this by using a modeless dialog as our main window:

```c++
BOOL CCustomDrawApp::InitInstance()
{
    CModelessDialog *dlg = new CModelessDialog;
    if (!dlg-&gt;Create())
	return FALSE;

    m_pMainWnd = dlg;

    return TRUE;
}

BOOL CModelessDialog::Create(CWnd* pParent /*=NULL*/)
{
    return CDialog::Create(CModelessDialog::IDD, pParent);
}
```
