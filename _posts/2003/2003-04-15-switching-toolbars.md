---
title: Switching Toolbars
date: 2003/04/15T11:20:39Z
---

MFC provides support for automatically switching menus when the active MDI child changes. Why doesn't it automatically
switch toolbars?

I don't know, but here's how you can do it. And it's easy.

Note that you'll have had to create your MFC application using "Internet Explorer ReBars".

## Where to put our hooks

The obvious thing to do is to find out how MFC decides to change the menus. We'll probably need to put our
toolbar-changing code in the same place.

The message sent to the MDI client window to change the menus is `WM_MDISETMENU`. If we do a quick find-in-files in the
MFC source tree for this, we find it sent from 4 places. The most promising of these is
`CMDIChildWnd::OnUpdateFrameMenu`. We'll figure out what it does, and decide if this is where our code ought to go.

The first thing that it does is work out which menu to use. Then, if this window is the one that's being activated, it
simply sets the new menu in place.

The next clause is what happens if we're destroying the last child window:

```c++
else if (hMenuAlt != NULL && !bActivate && pActivateWnd == NULL)
```

We have a menu (i.e. we changed it originally, and need to change it back). We're not being activated. In fact, nobody's
being activated. In this case, we set the menu back to the default.

The other clause seems to serve simply to refresh the menu.

## Show Me The Code

This looks good. We need to jump in here to swap out our toolbars. Fortunately, `OnUpdateFrameMenu` is virtual, so we
can override it in `CChildFrame`. The code is really simple:

```c++
void CChildFrame::OnUpdateFrameMenu(BOOL bActivate, CWnd *pActivateWnd, HMENU hMenuAlt)
{
    CMainFrame *pFrame = static_cast<CMainFrame *>(GetMDIFrame());

    if (bActivate)
        pFrame->ShowToolbar(m_nIDToolbar);
    else
        pFrame->HideToolbar(m_nIDToolbar);

    CMDIChildWnd::OnUpdateFrameMenu(bActivate, pActivateWnd, hMenuAlt);
}
```

We also need the `ShowToolbar` and `HideToolbar` functions. There's nothing particularly clever
here. They just keep a list of toolbars (and IDs), and show and hide them (creating them if necessary).

```c++
BOOL CMainFrame::ShowToolbar(UINT nIDToolbar)
{
    toolbars_t::const_iterator it = m_toolbars.find(nIDToolbar);
    if (it == m_toolbars.end())
    {
        CToolBar *pToolbar = NEW CToolBar;
        if (!pToolbar)
            return FALSE;

        if (!pToolbar->CreateEx(this) || !pToolbar->LoadToolBar(nIDToolbar) || !m_wndReBar.AddBar(pToolbar))
        {
            delete pToolbar;
            return FALSE;
        }

        m_toolbars.insert(std::make_pair(nIDToolbar, pToolbar));
    }
    else
    {
        CToolBar *pToolbar = it->second;
        pToolbar->ShowWindow(SW_SHOW);
    }

    RecalcLayout();

    return TRUE;
}

BOOL CMainFrame::HideToolbar(UINT nIDToolbar)
{
    toolbars_t::const_iterator it = m_toolbars.find(nIDToolbar);
    if (it == m_toolbars.end())
        return FALSE;

    CToolBar *pToolbar = it->second;
    pToolbar->ShowWindow(SW_HIDE);

    RecalcLayout();

    return TRUE;
}
```

That's about it for interesting stuff. Now for the mundane bits...

```c++
CChildFrame::CChildFrame()
    : m_nIDToolbar(-1)
{
}
```

```c++
BOOL CChildFrame::LoadFrame(UINT nIDResource, DWORD dwDefaultStyle, CWnd *pParentWnd, CCreateContext *pContext)
{
    m_nIDToolbar = nIDResource;

    return CMDIChildWnd::LoadFrame(nIDResource, dwDefaultStyle, pParentWnd, pContext);
}
```

```c++
class CChildFrame : public CMDIChildWnd
{
    UINT m_nIDToolbar;

    /* ... */

    {% raw %}//{{AFX_VIRTUAL(CChildFrame)
    public:
    virtual BOOL LoadFrame(UINT nIDResource, DWORD dwDefaultStyle, CWnd *pParentWnd, CCreateContext *pContext = NULL);
    virtual void OnUpdateFrameMenu(BOOL bActive, CWnd *pActivateWnd, HMENU hMenuAlt);
    //}}AFX_VIRTUAL{% endraw %}
};
```

```c++
void CMainFrame::OnDestroy()
{
    for (toolbars_t::iterator i = m_toolbars.begin(); i != m_toolbars.end(); ++i)
    {
        CToolBar *p = i->second;
        delete p;
    }

    m_toolbars.clear();

    CMDIFrameWnd::OnDestroy();
}
```

```c++
#include <map>

class CMainFrame : public CMDIFrameWnd
{
    typedef std::map<UINT, CToolBar *> toolbars_t;
    toolbars_t m_toolbars;

public:
    BOOL ShowToolbar(UINT nIDToolbar);
    BOOL HideToolbar(UINT nIDToolbar);

    /* ... */
};
```

Sample code is [here](toolbar_switch.zip).
