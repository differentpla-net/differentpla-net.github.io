---
title: "Using a modal or modeless dialog as a main window"
date: 2004-01-29T20:37:00.000Z
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
    //  application, rather than start the application's message pump.
    return FALSE;
}
```

## Using a modeless dialog as a main window

Unfortunately, this doesn't work properly if you have control bars in your dialog. This is because `DoModal` doesn't use the standard MFC message loop. This means that `OnIdle` is not called. If `OnIdle` is not called, then `WM_IDLEUPDATECMDUI` is not sent, and the control bars aren't updated correctly.

We solve this by using a modeless dialog as our main window:

```c++
BOOL CCustomDrawApp::InitInstance()
{
    CModelessDialog *dlg = new CModelessDialog;
    if (!dlg->Create())
	    return FALSE;

    m_pMainWnd = dlg;
    return TRUE;
}
```

```c++
BOOL CModelessDialog::Create(CWnd* pParent /*=NULL*/)
{
    return CDialog::Create(CModelessDialog::IDD, pParent);
}
```

To get this to work (otherwise the application doesn't exit when you press the OK or Cancel buttons), you'll also need to handle `OnOK` and `OnCancel`:

```c++
void CModelessDialog::OnOK()
{
    CDialog::OnOK();

    DestroyWindow();
}

void CModelessDialog::OnCancel()
{
    CDialog::OnOK();

    DestroyWindow();
}
```

And to fix the memory leak, handle `PostNcDestroy`:

```c++
void CModelessDialog::PostNcDestroy()
{
    CDialog::PostNcDestroy();

    delete this;
}
```
