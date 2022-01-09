---
title: "Using DrawDragRect to rubber-band a selection"
date: 2004-03-31T13:54:00.000Z
tags: mfc
---
If you're allowing the user to "rubber-band" a selection in your application, you might choose to use the `DrawDragRect` function. Here's how.

To use `DrawDragRect`, you pass it the a rectangle to highlight. You also pass it the previous rectangle that it highlighted, so that it can erase the old one before drawing the new one.

The code looks like this:

<pre>void CChildView::OnLButtonDown(UINT nFlags, CPoint point)
{
    CWnd::OnLButtonDown(nFlags, point);

    SetCapture();

    m_initialPoint = point;

    CRect rect(point, point);

    CDC *pDC = GetDC();
    pDC->DrawDragRect(&rect, CSize(1,1), NULL, CSize(1,1), NULL, NULL);
    m_lastRect = rect;
    ReleaseDC(pDC);
}

void CChildView::OnMouseMove(UINT nFlags, CPoint point)
{
    if (GetCapture() == this)
    {
        CRect rect(m_initialPoint, point);
        rect.NormalizeRect();

        CDC *pDC = GetDC();
        pDC->DrawDragRect(&rect, CSize(1,1), &m_lastRect, CSize(1,1), NULL, NULL);
        m_lastRect = rect;
        ReleaseDC(pDC);
    }

    CWnd::OnMouseMove(nFlags, point);
}

void CChildView::OnLButtonUp(UINT nFlags, CPoint point)
{
    if (GetCapture() == this)
    {
        CDC *pDC = GetDC();
        CRect rect(0,0,0,0);
        pDC->DrawDragRect(rect, CSize(1,1), &m_lastRect, CSize(1,1), NULL, NULL);
        ReleaseDC(pDC);

        ReleaseCapture();
    }

    CWnd::OnLButtonUp(nFlags, point);
}</pre>

There is (at least) one problem with this code, though: If the user Alt+Tabs away from your application, you'll lose mouse capture, but won't erase the selection rectangle.

To fix it, you need to add the following:

<pre>void CChildView::OnCancelMode()
{
    if (GetCapture() == this)
    {
        CDC *pDC = GetDC();
        CRect rect(0,0,0,0);
        pDC->DrawDragRect(rect, CSize(1,1), &m_lastRect, CSize(1,1), NULL, NULL);
        ReleaseDC(pDC);

        ReleaseCapture();
    }

    CWnd::OnCancelMode();
}</pre>
