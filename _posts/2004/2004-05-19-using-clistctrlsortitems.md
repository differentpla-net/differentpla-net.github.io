---
title: "Using CListCtrl::SortItems"
date: 2004-05-19T14:38:00.000Z
x-drupal-nid: 79
x-needs-review: 2004-05-19T14:38:00.000Z
tags: mfc
---
In my [previous article](/node/view/253), I explained how to use `LPSTR_TEXTCALLBACK` and `LVN_GETDISPINFO`.

This article will show how to use `SortItems` to sort the items in the list.

Why would we want to do this? If we'd just stored the text in the list control, rather than use `LPSTR_TEXTCALLBACK`, it would have sorted for us.

Well, yes it would, but it would have been wrong. In the last article, we used a variety of functions (e.g. `StrFormatKBSize`) to produce the strings that went into the list control. When sorting, we'd prefer to order the underlying values, not the formatted text.

This is where `SortItems` comes in.

## Calling SortItems

When you call `SortItems`, you need to pass a comparison function, like this:

<pre>int CALLBACK CompareFunction(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort);</pre>

`lParam1` and `lParam2` are the `lParam` values from the two items to be compared. `lParamSort` is user-defined data passed to `SortItems`. The return value of your comparison function should be like that of `strcmp`. That is: it should return a negative value if lhs < rhs, a positive value if lhs > rhs, or zero if lhs is equal to rhs.
Our comparison function looks like this:

<pre>int CALLBACK CompareFunction(LPARAM lParam1, LPARAM lParam2, LPARAM lParamSort)
{
    int iSubItem = (int)lParamSort;

    const FileItem *item1 = reinterpret_cast<const FileItem *>(lParam1);
    const FileItem *item2 = reinterpret_cast<const FileItem *>(lParam2);

    switch (iSubItem)
    {
    case SUBITEM_NAME:
        {
            CString name1 = item1->GetName();
            CString name2 = item2->GetName();

            return name1.CompareNoCase(name2);
        }

    case SUBITEM_SIZE:
        {
            LONGLONG size1 = item1->GetSize();
            LONGLONG size2 = item2->GetSize();

            return (size1 == size2) ? 0 : (size1 < size2) ? -1 : 1;
        }

    case SUBITEM_MODIFIED:
        {
            FILETIME mod1 = item1->GetModified();
            FILETIME mod2 = item2->GetModified();

            return CompareFileTime(&mod1, &mod2);
        }
    }

    return 0;
}</pre>

As you can see, we can use the same function for sorting on each of the different columns if we pass the sub-item ID in `lParamSort`. This function also demonstrates how to compare different types of data. Note that the string comparison is not technically correct: it doesn't take into account locale settings.

We call it from our `HDN_ITEMCLICK` handler:

<pre>void CListSortDlg::OnHdnItemclickListctrl(NMHDR *pNMHDR, LRESULT *pResult)
{
    LPNMHEADER phdr = reinterpret_cast<LPNMHEADER>(pNMHDR);

    int nCol = phdr->iItem;

    LVCOLUMN column;
    memset(&column, 0, sizeof(LVCOLUMN));
    column.mask = LVCF_SUBITEM;
    VERIFY(m_listCtrl.GetColumn(nCol, &column));
    int iSubItem = column.iSubItem;

    // We need to sort by iSubItem.
    VERIFY(m_listCtrl.SortItems(CompareFunction, iSubItem));

    *pResult = 0;
}</pre>
