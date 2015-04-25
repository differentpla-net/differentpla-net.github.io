---
title: "Grabbing a snapshot of the screen"
date: 2004-04-02T10:51:00.000Z
x-drupal-nid: 155
x-needs-review: 2004-04-02T10:51:00.000Z
---
Some (simple) code that shows how to grab a snapshot of the screen and display it in a window.

**Note:** Of course, if you simply want to take a snapshot of the screen to use in Microsoft Paint, just hit your `Print Screen` button: the screen grab will be put on the clipboard.

Something I'm working on at the moment requires some bitmap manipulation. Because it's been a long time since I read Petzold, I'm putting it together in pieces. This piece shows how to grab a snapshot of the screen and display it in a window.

<pre>void CGrabWnd::OnLButtonDown(UINT nFlags, CPoint point)
{
    CRect clientRect;
    GetClientRect(&clientRect);

    // We need the window DC.
    CWindowDC dc(this);

    // We need an offscreen DC.
    CDC memoryDC;
    memoryDC.CreateCompatibleDC(&dc);

    // We need a bitmap to back the bits.
    CBitmap memoryBitmap;
    memoryBitmap.CreateCompatibleBitmap(&dc,
                    clientRect.Width(), clientRect.Height());

    CBitmap *oldMemoryBitmap = memoryDC.SelectObject(&memoryBitmap);

    // BLOCK: Copy a chunk of the screen to the memory DC.
    {
        CWindowDC screenDC(NULL);
        memoryDC.BitBlt(0, 0, clientRect.Width(), clientRect.Height(),
                    &screenDC, 0, 0, SRCCOPY);
    }

    // Copy that to the client area.
    dc.BitBlt(0, 0, clientRect.Width(), clientRect.Height(),
                    &memoryDC, 0, 0, SRCCOPY);

    memoryDC.SelectObject(oldMemoryBitmap);

    CWnd::OnLButtonDown(nFlags, point);
}</pre>

There's nothing particularly clever here. One thing of note is that we used `CWindowDC` to grab hold of both the screen DC and the window DC. An alternative way to do this would have been:

<pre>CDC dc;
dc.Attach(::GetDC(NULL));

// some code

::ReleaseDC(NULL, dc.Detach());</pre>

Another thing worth commenting on is that this code copies the screen to an offscreen bitmap, and then to the client area, rather than copying it directly. The reason for this? I'm planning on altering the snapshot before displaying it.