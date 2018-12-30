---
title: "Direct access to a Bitmap using GetDIBits and SetDIBits"
date: 2004-04-02T11:28:00.000Z
redirect_from: /content/2004/04/direct-access-to-a-bitmap-using-getdibits-and-setdibits
---
In the [Grabbing a snapshot of the screen](/node/view/207) article, I said: "I'm planning on altering the snapshot before displaying it.". This article shows how to alter a bitmap by directly accessing the raw bits.

If we start with the code from the previous article, it's as simple as adding the following code:

<pre>// BLOCK: Copy a chunk of the screen to the memory DC.
{
    CWindowDC screenDC(NULL);
    memoryDC.BitBlt(0, 0, clientRect.Width(), clientRect.Height(),
                &screenDC, 0, 0, SRCCOPY);

    // Find out about the bitmap.
    BITMAP bitmapObject;
    memoryBitmap.GetBitmap(&bitmapObject);

    // Get the BITMAPINFO structure for the bitmap.
    BITMAPINFO *bmi = (BITMAPINFO *)_alloca(sizeof(BITMAPINFOHEADER)
                + 256*sizeof(RGBQUAD));
    memset(&bmi->bmiHeader, 0, sizeof(BITMAPINFOHEADER));
    bmi->bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
    int scanLineCount = GetDIBits(dc, memoryBitmap, 0,
                bitmapObject.bmHeight, NULL, bmi, DIB_RGB_COLORS);

    // Allocate a buffer for the bits.
    int imageBytes = dib::GetBitmapBytes(&bmi->bmiHeader);
    char *pSourceBits = (char *)malloc(imageBytes);

    // Get the bits.
    scanLineCount = GetDIBits(dc, memoryBitmap, 0,
                bitmapObject.bmHeight, pSourceBits, bmi, DIB_RGB_COLORS);

    // Munge them.
    ASSERT(bmi->bmiHeader.biBitCount == 32);

    // Iterate over the bits, munging them.
    dib::ApplyBitmapTransform(bmi, pSourceBits, pSourceBits,
                dib::RedShiftTransform());

    ::SetDIBits(dc, memoryBitmap, 0, bitmapObject.bmHeight,
                pSourceBits, bmi, DIB_RGB_COLORS);

    // We're done; free the memory.
    free(pSourceBits);
}</pre>

This shows one way to call `GetDIBits`. We call it twice: once to find out about the bitmap, and then again to copy the actual data.

We also need the implementations of `dib::ApplyBitmapTransform` and `dib::GetBitmapBytes`. They look like this:

<pre>namespace dib
{
    int GetBytesPerPixel(int depth)
    {
        return (depth==32 ? 4 : 3);
    }

    int GetBytesPerRow(int width, int depth)
    {
        int bytesPerPixel = GetBytesPerPixel(depth);
        int bytesPerRow = ((width * bytesPerPixel + 3) & ~3);

        return bytesPerRow;
    }

    int GetBitmapBytes(int width, int height, int depth)
    {
        return height * GetBytesPerRow(width, depth);
    }

    int GetBitmapBytes(const BITMAPINFOHEADER *bmih)
    {
        return GetBitmapBytes(bmih->biWidth, bmih->biHeight, bmih->biBitCount);
    }

    template <class Transform>
        void ApplyBitmapTransform(const BITMAPINFO *bmi,
                const void *pSourceBits, void *pDestBits, const Transform & trans)
    {
        ASSERT(bmi->bmiHeader.biBitCount == 32);

        const RGBQUAD *pSource = (const RGBQUAD *)pSourceBits;
        RGBQUAD *pDest = (RGBQUAD *)pDestBits;

        for (int y = 0; y < bmi->bmiHeader.biHeight; ++y)
        {
            for (int x = 0; x < bmi->bmiHeader.biWidth; ++x)
            {
                int offset = x + (bmi->bmiHeader.biWidth * y);

                RGBQUAD src = pSource[offset];
                RGBQUAD dst = trans(src);
                pDest[offset] = dst;
            }
        }
    }

    class ColourShiftTransform
    {
    protected:
        // Shift the channel down a little,
        // making sure that we don't wrap.
        BYTE Downshift(BYTE b, BYTE down_by) const
        {
            if (b > down_by)
                return b - down_by;

            return 0;
        }

        // Shift the channel up a little,
        // making sure that we saturate, rather than wrap.
        BYTE Upshift(BYTE b, BYTE up_by) const
        {
            if (b < (255 - up_by))
                return b + up_by;

            return 255;
        }
    };

    class RedShiftTransform : public ColourShiftTransform
    {
    public:
        RGBQUAD operator() (RGBQUAD src) const
        {
            RGBQUAD result;

            result.rgbBlue = Downshift(src.rgbBlue, 40);
            result.rgbGreen = Downshift(src.rgbGreen, 40);
            result.rgbRed = Upshift(src.rgbRed, 40);
            result.rgbReserved = src.rgbReserved;

            return result;
        }
    };
};</pre>
