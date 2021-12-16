---
title: "Drawing a drop shadow: using a mono bitmap"
date: 2004-05-11T09:24:00.000Z
x-drupal-nid: 159
x-needs-review: 2004-05-11T09:24:00.000Z
tags: mfc
---
In an [earlier article](/node/view/239), I showed how to draw a drop shadow with the help of the ImageList control.

It occurred to me last night that there's a simpler way to do the masking for the shadow.

In the previous article, we used a colour bitmap and a special ROP code to draw the selected brush on the output DC.

There's an easier way: if we use a mono bitmap, SRCCOPY will do the necessary magic; as long as we've called `SetBkColor` and `SetTextColor` appropriately.

<pre>void DrawImageListItem(CDC *pDC, int x, int y,
                            CImageList *piml, int nImage,
                            COLORREF crBack, bool bDropShadow, COLORREF crShadow)
{
    if (bDropShadow)
    {
        // This is quite simple:
        // We draw the mask in crShadow, offset slightly down & right.
        // We draw the item transparently, offset slightly up & left.

        // We need to know how big each item in the image list is.
        int cx, cy;
        ImageList_GetIconSize(piml->GetSafeHandle(), &cx, &cy);

        // Drawing the drop shadow in the appropriate colour is trickier:
        // we need to go via a memory DC.

        // We need a memory DC, backed with some (mono) bits.
        CDC memoryDC;
        VERIFY(memoryDC.CreateCompatibleDC(pDC));
        CBitmap memoryBitmap;
        VERIFY(memoryBitmap.CreateBitmap(cx, cy, 1, 1, NULL));
        CBitmap *oldMemoryBitmap = memoryDC.SelectObject(&memoryBitmap);

        // Draw the mask onto it.  Transparent parts of the image are white.
        // Opaque parts are black.
        piml->Draw(&memoryDC, nImage, CPoint(0, 0), ILD_MASK);

        pDC->SetBkColor(crBack);
        pDC->SetTextColor(crShadow);
        pDC->BitBlt(x + 1, y, cx, cy, &memoryDC, 0, 0, SRCCOPY);

        // Start cleaning up.
        memoryDC.SelectObject(oldMemoryBitmap);

        // That's given us a drop shadow.
        // All we need to do now is put the normal image over the top.
        piml->Draw(pDC, nImage, CPoint(x-1, y-1), ILD_NORMAL | ILD_TRANSPARENT);
    }
    else
    {
        // Then it's much, much simpler:
        // just get the image list to draw on the DC directly.
        piml->Draw(pDC, nImage, CPoint(x,y), ILD_NORMAL | ILD_TRANSPARENT);
    }
}</pre>

Essentially, when given a mono bitmap as the source, `BitBlt(..., SRCCOPY)` will treat black pixels as the foreground colour, and white pixels as the background colour.
