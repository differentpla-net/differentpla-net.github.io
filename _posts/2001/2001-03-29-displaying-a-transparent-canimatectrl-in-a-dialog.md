---
title: "Displaying a transparent CAnimateCtrl in a dialog"
date: 2001-03-29T23:00:00.000Z
tags: mfc
---
Microsoft's knowledge base article, [Q179907](http://support.microsoft.com/default.aspx?scid=kb;en-us;Q179907) explains how to use a transparent CAnimateCtrl in a CView or a CDialog.

Unfortunately, the instructions don't quite work properly for displaying the animation control over another control (e.g. a CListCtrl) in a dialog - the animation control uses the dialog colour (if you don't handle WM_CTLCOLOR) or white (if you do).

The fix is simple - just set the background colour of the DC:

```c++
    HBRUSH CMyDialog::OnCtlColor(CDC *pDC, CWnd *pWnd, UINT nCtlColor)
    {
        if (pWnd->GetDlgCtrlID() == IDC_ANIMATE)
        {
            pDC->SetBkColor(GetSysColor(COLOR_WINDOW));  // add this
            return (HBRUSH)GetStockObject(NULL_BRUSH);   // Q179907 says this
        }

        return CDialog::OnCtlColor(pDC, pWnd, nCtlColor);
    }
```

Apparently, this may not work with Visual Studio.NET and Windows XP. If this is the case, try handling `WM_CTLCOLORSTATIC`:

```c++
    LRESULT CMyDialog::WindowProc(UINT message, WPARAM wParam, LPARAM lParam)
    {
        if( message == WM_CTLCOLORSTATIC  && ::GetDlgCtrlID( (HWND)lParam ) == IDC_ANIMATE) )
        {
            CDC* pDC = CDC::FromHandle( (HDC)wParam );
            pDC->SetBkColor( GetSysColor(COLOR_WINDOW) );
            return (LRESULT)GetStockObject(NULL_BRUSH);
        }
        return CDialog::WindowProc(message, wParam, lParam);
    }
```

(Thanks to Mark Gullacher for sharing this with me).
Unfortunately, this doesn't work with Visual C++ 6 and Windows XP, using a manifest file: the background of the animation is painted black. You want this instead:

```c++
    class CMyDialog : public CDialog
    {
        //...
        CBrush m_background_brush;
        //...

        afx_msg HBRUSH OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor);
        afx_msg LRESULT OnCtlColorStatic(WPARAM wParam, LPARAM lParam);
    };

    BEGIN_MESSAGE_MAP(CMediaLibraryView, CSourceView)
        //...
        ON_WM_CTLCOLOR()
        ON_MESSAGE(WM_CTLCOLORSTATIC, OnCtlColorStatic)
    END_MESSAGE_MAP()

    BOOL CMyDialog::OnInitDialog()
    {
        //...
        m_background_brush.CreateSolidBrush(GetSysColor(COLOR_WINDOW));
        //...
    }

    HBRUSH CMyDialog::OnCtlColor(CDC* pDC, CWnd* pWnd, UINT nCtlColor)
    {
        if (pWnd->GetDlgCtrlID() == IDC_ANIMATE)
        {
            pDC->SetBkColor(GetSysColor(COLOR_WINDOW));  // add this
            pDC->SetBkMode(TRANSPARENT);
            return (HBRUSH)m_background_brush.GetSafeHandle();
        }
        else
            return CWizardPage::OnCtlColor(pDC, pWnd, nCtlColor);
    }

    LRESULT CMyDialog::OnCtlColorStatic(WPARAM wParam, LPARAM lParam)
    {
        HDC hDC = (HDC)wParam;
        HWND hwndCtl = (HWND)lParam;

        if (::GetDlgCtrlID(hwndCtl) == IDC_ANIMATE)
        {
            CDC *pDC = CDC::FromHandle(hDC);
            pDC->SetBkColor(GetSysColor(COLOR_WINDOW));
            pDC->SetBkMode(TRANSPARENT);
            return (LRESULT)(HBRUSH)m_background_brush.GetSafeHandle();
        }

        return Default();
    }
```
