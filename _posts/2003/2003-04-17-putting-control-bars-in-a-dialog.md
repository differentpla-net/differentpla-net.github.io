---
title: "Putting control bars in a dialog"
date: 2003-04-17T08:30:00.000Z
tags: mfc
---

There's some discussion of this in Microsoft Knowledge Base Article
[Q141751](http://support.microsoft.com/default.aspx?scid=kb;en-us;141751).

## Putting control bars in a dialog

This is initially quite simple:


```c++
class CControlBarDlg : public CDialog
{
    // See Q141751
    CDialogToolBar m_wndToolBar;
    CStatusBar m_wndStatusBar;
    // ...
```

```c++
int CControlBarDlg::OnCreate(LPCREATESTRUCT lpCreateStruct)
{
    if (CDialog::OnCreate(lpCreateStruct) == -1)
        return -1;

    if (!m_wndToolBar.CreateEx(this) ||
        !m_wndToolBar.LoadToolBar(IDR_MAINFRAME))
    {
        TRACE0("Failed to create toolbar\n");
        return -1;      // fail to create
    }

    if (!m_wndStatusBar.Create(this))
    {
        TRACE0("Failed to create statusbar\n");
        return -1;
    }

    return 0;
}
```

## Using RepositionBars

```c++
void CControlBarDlg::OnSize(UINT nType, int cx, int cy)
{
    RecalcLayout();

    CDialog::OnSize(nType, cx, cy);
}

void CControlBarDlg::RecalcLayout()
{
    CRect client;
    GetClientRect(&client);

    CRect remaining;
    RepositionBars(0, 0xffff, AFX_IDW_PANE_FIRST, CWnd::reposQuery, &remaining, &client);
    RepositionBars(0, 0xffff, AFX_IDW_PANE_FIRST, CWnd::reposDefault, NULL, &client);

    // TODO: reposition the other controls, using _remaining_
}
```

## Using a status bar to provide sizing

It's magic -- it just works. If you put a status bar in your dialog, and position it correctly, the user can resize your
window using the size box on its corner.
