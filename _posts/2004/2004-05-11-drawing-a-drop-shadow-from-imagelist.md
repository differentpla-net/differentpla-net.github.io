---
title: "Drawing an item from an Image List with a drop shadow"
date: 2004-05-11T09:24:00.000Z
redirect_from: /node/view/239
tags: mfc
---
The documentation for `ImageList_DrawIndirect` seems to imply that it can draw an item from an ImageList control with a drop shadow.  Unfortunately, it doesn't seem to be supported.

Here's a snippet of code that does it.

```c++
void DrawImageListItem(CDC *pDC, int x, int y,
                    CImageList *piml, int nImage, bool bDropShadow)
{
    if (bDropShadow)
    {
        // This is quite simple:
        // We draw the mask in COLOR_3DSHADOW, offset slightly
        // down & right.
        // We draw the item transparently, offset slightly
        // up & left.

        // We need to know how big each item in the image list is:
        int cx, cy;
        ImageList_GetIconSize(piml->GetSafeHandle(), &cx, &cy);

        // Drawing the drop shadow in the appropriate colour is trickier:
        // we need to go via a memory DC.
        
        // We need a memory DC, backed with some bits.
        CDC memoryDC;
        VERIFY(memoryDC.CreateCompatibleDC(pDC));
        CBitmap memoryBitmap;
        VERIFY(memoryBitmap.CreateCompatibleBitmap(pDC, cx, cy));
        CBitmap *oldMemoryBitmap = memoryDC.SelectObject(&memoryBitmap);

        // Draw the mask onto it.
        // Transparent parts of the image are white.
        // Opaque parts are black.
        piml->Draw(&memoryDC, nImage, CPoint(0, 0), ILD_MASK);

        // This raster code does the necessary magic:
        // Where there's a black pixel in the source DC,
        // use the current brush.
        // Where there's a white pixels leave the destination alone.
        const DWORD CP_ROP = 0xB8074A;

        // So, we need a brush:
        CBrush br;
        br.CreateSolidBrush(GetSysColor(COLOR_3DSHADOW));

        // Do the magic
        CBrush *oldBrush = pDC->SelectObject(&br);
        pDC->BitBlt(x + 1, y, cx, cy, &memoryDC, 0, 0, CP_ROP);
        pDC->SelectObject(oldBrush);

        // Start cleaning up.
        memoryDC.SelectObject(oldMemoryBitmap);

        // That's given us a drop shadow.
        // All we need to do now is put the normal image over the top.
        piml->Draw(pDC, nImage, CPoint(x-1, y-1),
                        ILD_NORMAL | ILD_TRANSPARENT);
    }
    else
    {
        // Then it's much, much simpler: just get the
        // image list to draw on the DC directly.
        piml->Draw(pDC, nImage, CPoint(x,y),
                        ILD_NORMAL | ILD_TRANSPARENT);
    }
}
```

Note that this draws the normal image at (x,y).  The shadowed variant is drawn starting one pixel to the left and higher.  Don't forget to take this into account when telling it where to draw the image.
