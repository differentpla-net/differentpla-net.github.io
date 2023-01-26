---
title: "Self-drawing CListBox, LBS_OWNERDRAWFIXED and subclassing"
date: 2004-05-28T14:31:00.000Z
redirect_from:
- /node/view/262
---

If you've got a `LISTBOX` (as opposed to a `ListView`) control in a dialog, and you're using `LBS_OWNERDRAWFIXED`, you might be surprised to find that your `MeasureItem` function is never called.

This is because the original `WM_MEASUREITEM` message is sent before the subclassing is finished. It doesn't, therefore, get reflected to your `CListBox`-derived class.

The solution?

One solution is to use `LBS_OWNERDRAWVARIABLE`, so that you get called for each item. You can always return the same value for each. Another solution is to use the `SetItemHeight` function to set the height.

The same limitations apply to `COMBOBOX` controls created with the `CBS_OWNERDRAWFIXED` style.

Or you could do something devious to hide this nastiness:

```c++
void COwnerDrawListBox::PreSubclassWindow()
{
    CListBox::PreSubclassWindow();

    if (GetStyle() & LBS_OWNERDRAWFIXED)
    {
        MEASUREITEMSTRUCT measureItem;

        // MeasureItem will probably ignore these,
        // but we'll fill them in anyway:
        measureItem.CtlID = GetDlgCtrlID();
        measureItem.CtlType = ODT_LISTBOX;
        measureItem.itemID = -1;
        measureItem.itemData = 0;

        // MeasureItem is supposed to fill these in,
        // but we'll put some defaults in there:
        measureItem.itemWidth = 0;
        measureItem.itemHeight = 0;

        MeasureItem(&measureItem);

        // For LBS_OWNERDRAWFIXED, nIndex should be zero.
        SetItemHeight(0, measureItem.itemHeight);
    }
}
```
