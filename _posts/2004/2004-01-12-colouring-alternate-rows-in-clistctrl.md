---
title: "Colouring alternate rows in a CListCtrl"
date: 2004-01-12T12:23:00.000Z
redirect_from: /node/view/63
tags: mfc
---

Nothing particularly strenuous: Just handle `NM_CUSTOMDRAW` and play with `clrTextBk`. The only real surprise is that you have to return `CDRF_NEWFONT` from your handler to get the new colour used.

```c++
BEGIN_MESSAGE_MAP(CMyListCtrl, CListCtrl)
    {% raw %}//{{AFX_MSG_MAP(CMyListCtrl)
    ON_NOTIFY_REFLECT(NM_CUSTOMDRAW, OnCustomDraw)
    //}}AFX_MSG_MAP{% endraw %}
END_MESSAGE_MAP()

void CMyListCtrl::OnCustomDraw(NMHDR *pNMHDR, LRESULT *pResult)
{
    NMLVCUSTOMDRAW *customDraw = reinterpret_cast<NMLVCUSTOMDRAW *>(pNMHDR);

    *pResult = OnCustomDraw(customDraw);
}

LRESULT CMyListCtrl::OnCustomDraw(NMLVCUSTOMDRAW *customDraw)
{
    DWORD dwStyle = GetStyle() & LVS_TYPEMASK;

    if (dwStyle == LVS_REPORT)
    {
        switch (customDraw->nmcd.dwDrawStage)
        {
        case CDDS_PREPAINT:
            return CDRF_NOTIFYITEMDRAW;

        case CDDS_ITEMPREPAINT:
            if ((customDraw->nmcd.dwItemSpec % 2) == 0)
            {
                customDraw->clrTextBk = RGB(0xCC, 0xFF, 0xFF);
                return CDRF_NEWFONT;
            }

            return CDRF_DODEFAULT;
        }
    }

    return CDRF_DODEFAULT;
}
```
