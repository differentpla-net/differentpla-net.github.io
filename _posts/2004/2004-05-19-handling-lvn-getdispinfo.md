---
title: "Handling LVN_GETDISPINFO"
date: 2004-05-19T14:24:00.000Z
redirect_from: /node/view/253
tags: mfc
---

If you specify `LPSTR_TEXTCALLBACK` when inserting an item into a ListView control, it no longer supports sorting; you can't click on the column heading to sort the list control.

You should handle `HDN_ITEMCLICK` and use the `CListCtrl::SortItems` function instead.  Here's how.

The first thing we need is to populate the list control with some data.  To demonstrate that we can do more than simple string sorting, we'll use some other stuff:

```c++
// Put some columns in.
m_listCtrl.InsertColumn(0, _T("Name"),
                LVCFMT_LEFT, 100, SUBITEM_NAME);
m_listCtrl.InsertColumn(1, _T("Size"),
                LVCFMT_RIGHT, 100, SUBITEM_SIZE);
m_listCtrl.InsertColumn(2, _T("Modified"),
                LVCFMT_LEFT, 200, SUBITEM_MODIFIED);

// Enumerate the files in the current directory.
WIN32_FIND_DATA ffd;
HANDLE hff = FindFirstFile(_T("*"), &ffd);
if (hff != INVALID_HANDLE_VALUE)
{
    do
    {
        if (IsSpecialDirectory(ffd.cFileName))
            continue;

        if (!(ffd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY))
        {
            LVITEM item;
            memset(&item, 0, sizeof(LVITEM));
            item.mask = LVIF_TEXT | LVIF_PARAM;
            item.pszText = LPSTR_TEXTCALLBACK;

            FileItem *pItem = new FileItem(&ffd);
            item.lParam = reinterpret_cast<LPARAM>(pItem);

            int nItem = m_listCtrl.InsertItem(&item);
            ASSERT(nItem != -1);
        }
    } while (FindNextFile(hff, &ffd));

    FindClose(hff);
}
```

This doesn't do anything particularly interesting.  It adds three columns to our list: "Name", "Size" and "Modified".  It then enumerates the files in the current directory and calls `InsertItem` for each one.

We stash a `FileItem` object in `lParam`. `FileItem` looks like this:

```c++
class FileItem
{
    WIN32_FIND_DATA m_findData;

public:
    FileItem(const WIN32_FIND_DATA *pFindData)
        : m_findData(*pFindData)
    {
    }

    CString GetName() const { return m_findData.cFileName; }

    LONGLONG GetSize() const
    {
        ULARGE_INTEGER uli;
        uli.HighPart = m_findData.nFileSizeHigh;
        uli.LowPart = m_findData.nFileSizeLow;

        return uli.QuadPart;
    }

    FILETIME GetModified() const { return m_findData.ftLastWriteTime; }
};
```

Because we specify `LPSTR_TEXTCALLBACK` in `InsertItem`, we need to implement a handler for `LVN_GETDISPINFO`.  This looks like this:

```c++
void CListSortDlg::OnLvnGetdispinfoListctrl(NMHDR *pNMHDR, LRESULT *pResult)
{
    NMLVDISPINFO *pDispInfo = reinterpret_cast<NMLVDISPINFO*>(pNMHDR);

    const FileItem *pItem = reinterpret_cast<const FileItem *>(pDispInfo->item.lParam);

    UINT mask = pDispInfo->item.mask;
    if (mask & LVIF_TEXT)
    {
        CString strText;

        // This isn't actually the subitem:
        int nCol = pDispInfo->item.iSubItem;

        LVCOLUMN column;
        memset(&column, 0, sizeof(LVCOLUMN));
        column.mask = LVCF_SUBITEM;
        VERIFY(m_listCtrl.GetColumn(nCol, &column));
        int iSubItem = column.iSubItem;

        switch (iSubItem)
        {
        case SUBITEM_NAME:
            strText = pItem->GetName();
            break;

        case SUBITEM_SIZE:
            {
                StrFormatKBSize(pItem->GetSize(), strText.GetBuffer(MAX_PATH), MAX_PATH);
                strText.ReleaseBuffer();
            }
            break;

        case SUBITEM_MODIFIED:
            {
                FILETIME ft = pItem->GetModified();

                SYSTEMTIME st;
                VERIFY(FileTimeToSystemTime(&ft, &st));

                CString strDate;
                GetDateFormat(LOCALE_USER_DEFAULT, DATE_LONGDATE,
                                &st, NULL, strDate.GetBuffer(_MAX_PATH), _MAX_PATH);
                strDate.ReleaseBuffer();

                CString strTime;
                GetTimeFormat(LOCALE_USER_DEFAULT, 0,
                                &st, NULL, strTime.GetBuffer(_MAX_PATH), _MAX_PATH);
                strTime.ReleaseBuffer();

                strText.Format(_T("%s %s"), (LPCTSTR)strDate, (LPCTSTR)strTime);
            }
            break;
        }

        // Make sure that we don't overflow the buffer.
        _tcsncpy(pDispInfo->item.pszText, strText, pDispInfo->item.cchTextMax);

        // Because _tcsncpy won't put the NULL terminator in the
        // buffer if we're right
        // at the end, we need to do it ourselves:
        pDispInfo->item.pszText[pDispInfo->item.cchTextMax-1] = _T('\0');
    }

    *pResult = 0;
}
```

As you can see, this code looks at which sub-item is requested (allowing for the fact that <a href="http://www.differentpla.net/node/view/252">the documentation lies</a>) and returns the relevant value as a string.

We need to also be careful about overflowing the `pszText` buffer.  See <a href="http://weblogs.asp.net/oldnewthing/archive/2004/01/21/61101.aspx">this post</a> on Raymond Chen's blog</a> for more information.

Taken by itself, there's nothing particularly interesting here, but it will be helpful background to my next article, which talks about using `CListCtrl::SortItems`.
