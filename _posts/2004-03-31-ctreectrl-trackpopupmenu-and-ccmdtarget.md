---
title: "CTreeCtrl, TrackPopupMenu and CCmdTarget"
date: 2004-03-31T17:17:00.000Z
x-drupal-nid: 153
x-needs-review: 2004-03-31T17:17:00.000Z
---
Some twisted code showing how to combine MFC's `CTreeCtrl`, `CCmdTarget` and `TrackPopupMenu`.

Let's assume that you want to allow your users to get a context menu when they right-click on a tree control. The first part is simple. You can subclass the control (which is easy in MFC) and handle WM_CONTEXTMENU. Alternatively, if you don't want to subclass the control, you can handle the NM_RCLICK notification that you'll receive from the tree control:

## Handling a right-click on the tree background

<pre>BEGIN_MESSAGE_MAP(CTreeCtrlDlg, CDialog)
    ON_NOTIFY(NM_RCLICK, IDC_TREECTRL, OnNMRclickTreectrl)
END_MESSAGE_MAP()</pre>

<pre>void CTreeCtrlDlg::OnNMRclickTreectrl(NMHDR *pNMHDR, LRESULT *pResult)
{
    *pResult = 0;

    CPoint ptScreen;
    if (GetCursorPos(&ptScreen))
    {
        CPoint ptClient(ptScreen);
        m_treeCtrl.ScreenToClient(&ptClient);

        UINT flags;
        HTREEITEM hTreeItem = m_treeCtrl.HitTest(ptClient, &flags);

        // The user hasn't clicked on any item.
        if (hTreeItem == NULL)
        {
            CMenu menu;
            VERIFY(menu.LoadMenu(IDR_POPUP_NOWHERE));
            CMenu *popup = menu.GetSubMenu(0);
            ASSERT(popup);
            VERIFY(popup->TrackPopupMenu(TPM_LEFTALIGN | TPM_RIGHTBUTTON,
                    ptScreen.x, ptScreen.y, this, NULL));
        }
    }
}</pre>

There are a few things to note here. The first is that the NM_RCLICK notification sent from a tree control doesn't include the mouse coordinates, so we have to get them ourselves. `GetCursorPos` returns the coordinates relative to the screen, so we have to convert them to client coordinates before calling `CTreeCtrl::HitTest`. `TrackPopupMenu` wants the coordinates in screen coordinates.
When the user selects a command, it'll be sent to the window passed to `TrackPopupMenu` in a `WM_COMMAND` message, which is exactly what we want.

## Handling right-click on an item

This code shows how to handle a right-click on the background of the tree control. What if we want to display a different popup menu if the user clicks on an item? That's easy:

<pre>        if (hTreeItem && (flags & TVHT_ONITEM))
        {
            CMenu menu;
            VERIFY(menu.LoadMenu(IDR_POPUP_ONITEM));
            CMenu *popup = menu.GetSubMenu(0);
            ASSERT(popup);
            VERIFY(popup->TrackPopupMenu(TPM_LEFTALIGN | TPM_RIGHTBUTTON,
                    ptScreen.x, ptScreen.y, this, NULL);</pre>

One problem with this approach is that the command will be sent to the dialog, and we'll have forgotten which item the user clicked on. There are several possible solutions to this problem. Among them:

*   Remember which item was clicked on. Add a handler for the command to the dialog. In the command handler, we can look at which item was clicked previously do decide what to do. This means carrying around a piece of data (the `HTREEITEM`) between messages, which is a smell.
*   Call `CTreeCtrl::SelectItem` to make the clicked item current, making the handler (again in the dialog) simpler. This is the simplest option, but has its own disadvantages -- what if selecting the item causes a bunch of unnecessary work? Imagine if we enumerated the contents of the folder, only to find that the user had selected the "Delete" menu item.
*   Pass the `TPM_RETURNCMD` flag to `TrackPopupMenu`.

This last option looks like this:

<pre>        UINT nCode = popup->TrackPopupMenu(TPM_LEFTALIGN | TPM_RIGHTBUTTON |
                        TPM_RETURNCMD,
                        ptScreen.x, ptScreen.y, this, NULL);
        if (nCode != 0)
        {
            /* do something with HTREEITEM and nCode. */
        }</pre>

Now, what if we wanted to create a slightly more MFC-oriented solution, and make it a little more object-oriented? It's a little bit of a smell to have the dialog handling commands (even in this way) for some other object (namely the item in the tree control.

## Using CCmdTarget and OnCmdMsg

This can be easily resolved by creating a new class, `CTreeCtrlItem` (or whatever you'd prefer to call it), derived from `CCmdTarget`, and putting these in the tree control, like this:

<pre>class CTreeCtrlItem : public CCmdTarget
{
    /* stuff */
};</pre>

<pre>    CTreeCtrlItem *pItem = new CTreeCtrlItem;
    LPARAM lParam = reinterpret_cast<LPARAM>(pItem);
    m_treeCtrl.InsertItem(TVIF_TEXT | TVIF_PARAM, "Root Item",
                            0, 0, 0, 0,
                            lParam, TVI_ROOT, TVI_LAST);</pre>

Then, when the user right-clicks on the tree control, you can get back the item and forward the message to it:

<pre>UINT nCode = popup->TrackPopupMenu(TPM_LEFTALIGN | TPM_RIGHTBUTTON |
                        TPM_RETURNCMD,
                        ptScreen.x, ptScreen.y, this, NULL);
if (nCode != 0)
{
    DWORD_PTR lParam = m_treeCtrl.GetItemData(hTreeItem);
    CCmdTarget *pItem = reinterpret_cast<CCmdTarget *>(lParam);
    ASSERT(pItem && pItem->IsKindOf(RUNTIME_CLASS(CCmdTarget)));
    pItem->OnCmdMsg(nCode, CN_COMMAND, NULL, NULL);
}</pre>

And, if you start adding command handlers to the `CTreeCtrlItem` class, they'll be handled correctly. The magic of this, of course, is that the objects that you put in the tree only need to be derived from `CCmdTarget` in order for this to work, meaning that you can put different objects in there, and the commands will be handled polymorphically, and routed correctly.

Source code is [here](http://www.differentpla.net/node/view/203).