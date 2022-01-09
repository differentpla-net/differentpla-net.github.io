---
title: "Displaying a tooltip when the user drags the scroll thumb"
date: 2004-01-12T12:24:00.000Z
tags: mfc
---
## You wanna do this?

![[img_assist|nid=47|title=|desc=|link=none|align=left|width=377|height=640]](/broken-image-link)

## Here's How:

**Note:** For details on how to do the full-row colouring, see [this](/node/view/63).

The first thing to do is to intercept the relevant scrollbar messages. This involves adding code to handle `WM_VSCROLL`, and looking for `SB_THUMBTRACK` and `SB_THUMBPOSITION`. In the sample code, we'll assume that you've got a class `CMyListCtrl`, derived from `CListCtrl`.

`SB_THUMBTRACK` is sent while the user is dragging the thumb. We want to display the tooltip in response. `SB_THUMBPOSITION` is sent when the user has finished dragging the thumb. We want to hide the tooltip.

```c++
void CMyListCtrl::OnVScroll(UINT nSBCode, UINT nPos, CScrollBar* pScrollBar)
{
    SCROLLINFO si;
    GetScrollInfo(SB_VERT, &si, SIF_POS);

    if (nSBCode == SB_THUMBTRACK)
    {
        CString s = FigureText(si.nPos);
        DisplayThumbTrackToolTip(s);
    }
    else if (nSBCode == SB_THUMBPOSITION)
    {
        DestroyThumbTrackToolTip();
    }

    CListCtrl::OnVScroll(nSBCode, nPos, pScrollBar);
}
```

Let's look at `DisplayThumbTrackToolTip` first. It creates the tooltip if it's not already created (using `CreateThumbTrackToolTip`). Since we're trying to right-align the tooltip, we have to use `TTM_TRACKACTIVATE` and `TTM_TRACKPOSITION` to make the tooltip appear in a specific location. However, this causes flickering when we set the new text and then move the tooltip to the correct place. To solve this, we use a gnarly hack: we move the tooltip off the screen.

According to some of the documentation, you have to ensure that the string you pass to UpdateTipText remains in scope until you've finished with the tooltip. We use `strdup` to make a copy, and then keep a pointer to it in `m_tooltipText`. This requires that we delete any previous text before we call `strdup` on the new text. We could probably keep the text around in a CString member, but I didn't try that. We then set the text and make the call to adjust the tooltip position.

```c++
void CMyListCtrl::DisplayThumbTrackToolTip(const char *toolTipText)
{
    if (!CreateThumbTrackToolTip())
        return;

    /* Move the tooltip off the screen, so that it doesn't flicker when
        * it's resized with the new text.
        */
    int xOffScreen = -4000;
    int yOffScreen = -4000;
    m_pToolTipCtrl->SendMessage(TTM_TRACKPOSITION, 0, MAKELONG(xOffScreen, yOffScreen));

    TOOLINFO ti;
    FillToolInfo(&ti);
    m_pToolTipCtrl->SendMessage(TTM_TRACKACTIVATE, TRUE, (LPARAM)&ti);

    if (m_tooltipText)
        free(m_tooltipText);

    m_tooltipText = strdup(toolTipText);
    m_pToolTipCtrl->UpdateTipText(m_tooltipText, this);

    AdjustThumbTrackToolTipPosition();
}
```

Let's take a quick look at the code that creates the CToolTipCtrl. There's nothing particularly interesting in here, with the exception of the stuff to set the delay times.

```c++
BOOL CMyListCtrl::CreateThumbTrackToolTip()
{
    if (m_pToolTipCtrl)
        return TRUE;	// Already created.

    m_pToolTipCtrl = NEW CToolTipCtrl;
    if (m_pToolTipCtrl && m_pToolTipCtrl->Create(this))
    {
        TOOLINFO ti;
        FillToolInfo(&ti);

        // We have to use SendMessage, because CToolTipCtrl::AddTool doesn't pass all of the flags.
        m_pToolTipCtrl->SendMessage(TTM_ADDTOOL, 0, (LPARAM)&ti);
        m_pToolTipCtrl->SetDelayTime(TTDT_AUTOPOP, SHRT_MAX);   // stop the tooltip coming up automatically
        m_pToolTipCtrl->SetDelayTime(TTDT_INITIAL, 0);
        return TRUE;	// Created successfully.
    }

    // Something failed.  Clean up and leave.
    delete m_pToolTipCtrl;
    return FALSE;
}
```

You can see `FillToolInfo` here. It's basically copied verbatim from `CToolTipCtrl::AddTool()`, with the addition of the `TTF_ABSOLUTE` and `TTF_TRACK` flags.

```c++
void CMyListCtrl::FillToolInfo(TOOLINFO *ti)
{
    memset(ti, 0, sizeof(TOOLINFO));
    ti->cbSize = sizeof(TOOLINFO);
    ti->hwnd = GetParent()->GetSafeHwnd();
    ti->uFlags = TTF_IDISHWND | TTF_ABSOLUTE | TTF_TRACK;
    ti->uId = (UINT)GetSafeHwnd();
}
```

The next piece of interesting code is `AdjustThumbTrackToolTipPosition()`. It moves the tooltip so that it's vertically aligned with the mouse pointer, and so that its right edge is inset from the right-hand side of the listview control.

```c++
void CMyListCtrl::AdjustThumbTrackToolTipPosition()
{
    CRect toolTipRect;
    m_pToolTipCtrl->GetWindowRect(&toolTipRect);

    CRect thisRect;
    GetWindowRect(&thisRect);

    CPoint pt;
    GetCursorPos(&pt);

    int xOffset = GetSystemMetrics(SM_CXVSCROLL) + GetSystemMetrics(SM_CXDLGFRAME);

    int x = thisRect.right - toolTipRect.Width();
    x -= xOffset;
    int y = pt.y;

    m_pToolTipCtrl->SendMessage(TTM_TRACKPOSITION, 0, MAKELONG(x, y));
}
```

The code to destroy the tooltip (when handling SB_THUMBPOSITION):

```c++
void CMyListCtrl::DestroyThumbTrackToolTip()
{
    if (m_pToolTipCtrl)
    {
        m_pToolTipCtrl->DestroyWindow();
        delete m_pToolTipCtrl;
        m_pToolTipCtrl = NULL;
    }

    if (m_tooltipText)
    {
        free(m_tooltipText);
        m_tooltipText = NULL;
    }
}
```

That's about it for interesting code. There just remains the miscellaneous bits:

```c++
class CMyListCtrl : public CListCtrl
{
    // ...

private:
    CToolTipCtrl *m_pToolTipCtrl;
    char *m_tooltipText;
};
```

```c++
CMyListCtrl::CMyListCtrl()
    : m_pToolTipCtrl(NULL), m_tooltipText(NULL)
{
}
```

```c++
BOOL CMyListCtrl::PreTranslateMessage(MSG* pMsg)
{
    if (m_pToolTipCtrl)
        m_pToolTipCtrl->RelayEvent(pMsg);

    return CListCtrl::PreTranslateMessage(pMsg);
}
```

```c++
void CMyListCtrl::OnDestroy()
{
    DestroyThumbTrackToolTip();

    CListCtrl::OnDestroy();
}
```

## That's it!

Of course, you have to go and implement `CString CMyListCtrl::FigureText(int nPos)`, but that's your problem.

Source code [is attached](/broken-link-to-source-code).
