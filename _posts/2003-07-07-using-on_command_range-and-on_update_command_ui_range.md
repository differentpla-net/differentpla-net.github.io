---
title: "Using ON_COMMAND_RANGE and ON_UPDATE_COMMAND_UI_RANGE"
date: 2003-07-07T17:13:00.000Z
x-drupal-nid: 59
x-needs-review: 2003-07-07T17:13:00.000Z
tags: mfc
---
The ON_COMMAND_RANGE and ON_UPDATE_COMMAND_UI_RANGE macros are useful when you want to treat a group of commands similarly. In this case, it's the commands for changing list view style. The command IDs must be contiguous, and you must specify the lower one first.

<div class="snippet">
{% raw %}
    BEGIN_MESSAGE_MAP(CCustomDrawDlg, CDialog)
        //{{AFX_MSG_MAP(CCustomDrawDlg)
        //...
        //}}AFX_MSG_MAP
        // ClassWizard can't handle these, so they go outside the AFX_MSG_MAP
        ON_UPDATE_COMMAND_UI_RANGE(ID_VIEW_SMALLICON, ID_VIEW_DETAILS, OnUpdateViewStyle)
        ON_COMMAND_RANGE(ID_VIEW_SMALLICON, ID_VIEW_DETAILS, OnViewStyle)
    END_MESSAGE_MAP()
{% endraw %}
</div>

<div class="snippet">
    void CCustomDrawDlg::OnViewStyle(UINT nID)
    {
        DWORD dwStyle = -1;

        switch (nID)
        {
        case ID_VIEW_DETAILS:
    	dwStyle = LVS_REPORT;
    	break;

        case ID_VIEW_SMALLICON:
    	dwStyle = LVS_SMALLICON;
    	break;

        case ID_VIEW_LARGEICON:
    	dwStyle = LVS_ICON;
    	break;

        case ID_VIEW_LIST:
    	dwStyle = LVS_LIST;
    	break;
        }

        // change the style; window will repaint automatically
        if (dwStyle != -1)
            m_listCtrl.ModifyStyle(LVS_TYPEMASK, dwStyle, 0);
    }

    void CCustomDrawDlg::OnUpdateViewStyle(CCmdUI* pCmdUI)
    {
        DWORD dwStyle = m_listCtrl.GetStyle();
        dwStyle &= LVS_TYPEMASK;

        pCmdUI->Enable();
        BOOL bChecked = FALSE;

        switch (pCmdUI->m_nID)
        {
        case ID_VIEW_DETAILS:
    	bChecked = (dwStyle == LVS_REPORT);
    	break;

        case ID_VIEW_SMALLICON:
    	bChecked = (dwStyle == LVS_SMALLICON);
    	break;

        case ID_VIEW_LARGEICON:
    	bChecked = (dwStyle == LVS_ICON);
    	break;

        case ID_VIEW_LIST:
    	bChecked = (dwStyle == LVS_LIST);
    	break;

        default:
    	bChecked = FALSE;
    	break;
        }

        pCmdUI->SetRadio(bChecked ? 1 : 0);
    }

</div>
