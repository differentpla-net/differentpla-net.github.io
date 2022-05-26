---
title: "Disabling the Cancel button in a Wizard"
date: 2004-01-09T13:03:00.000Z
tags: mfc
---
`CPropertySheet` provides the `SetWizardButtons` function, allowing you to enable or disable the "Back" or "Next" buttons. It doesn't, however, allow you to disable the "Cancel" button.

Here's how:

```c++
    CWnd *pCancel = GetParent()->GetDlgItem(IDCANCEL);
    if (pCancel)
    	pCancel->EnableWindow(FALSE);
```
