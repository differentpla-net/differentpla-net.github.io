---
title: "Cancelling Long-Lived Tasks from a Wizard"
date: 2004-02-27T10:23:00.000Z
x-drupal-nid: 187
x-needs-review: 2004-02-27T10:23:00.000Z
tags: mfc
---
This article, a follow-up to [this one]({% post_url 2004-01-08-displaying-progress-in-a-wizard %}), shows how to modify our project to allow the user to cancel the long-lived operation.

There are at least three different ways to cancel a long-lived task:

1.  Returning a failure code from the progress callback.
2.  Similarly, a separate `CheckCancel` callback.
3.  Calling a cancellation function from the callback, or from another thread.

Here, I'll consider the first case: returning a failure code from the progress callback.
So, we need to modify our [existing code](/node/view/133) as follows:

## Adding a return value to the progress callback

Simple, this one:

```
class TaskObserver
{
public:
	// Return true to keep going.
	virtual bool OnProgress(int current, int maximum) = 0;
	virtual void OnComplete() = 0;
};

inline bool Task::ReportProgress(int current, int maximum)
{
	if (m_pObserver)
		return m_pObserver->OnProgress(current, maximum);

	// By default, keep going.
	return true;
}
```

Obviously, the return value of the progress callback doesn't have to be `bool`. You could use an `HRESULT`, for example.

## Checking the Cancel button

We also need to change the progress callback so that it returns `true` or `false`:

```
bool CProgressPage::OnProgress(int current, int maximum)
{
	PumpMessages();

	m_progressCtrl.SetRange32(0, maximum);
	m_progressCtrl.SetPos(current);

	if (CheckCancelButton())
		return false;	// Stop

	return true;	// Keep going.
}
```

The call to `CheckCancelButton` returns `true` if the Cancel button has been pressed.
`CheckCancelButton` is as follows:

```
bool CProgressPage::CheckCancelButton()
{
	PumpMessages();

	// Reset m_bCancel to FALSE so that CheckCancelButton
	// returns FALSE until the user clicks Cancel again.
	// This will allow you to call CheckCancelButton and
	// still continue the operation.
	// If m_bCancel stayed TRUE, then the next call to
	// CheckCancelButton would always return TRUE

	bool bResult = m_bCancel;
	m_bCancel = FALSE;

	return bResult;
}
```

We also need some way of setting `m_bCancel` when the Cancel button is pressed. In a normal dialog, we'd handle `IDCANCEL`. In a property page, we have to do something a little different:

```
/** Called when the Cancel button is pressed.
 * Return FALSE to prevent cancellation.
 * Remember that the button was pressed for CheckCancelButton.
 */
BOOL CProgressPage::OnQueryCancel()
{
	m_bCancel = true;
	return FALSE;
}
```

We also need to modify the long-lived operation to check the progress callback result:

```
bool b = ReportProgress(i, MAX_ITERATIONS);
if (!b)
	break;
```

And the Cancel button needs to be enabled:

```
LRESULT CProgressPage::OnStartTask(WPARAM /*wParam*/, LPARAM /*lParam*/)
{
	CWaitCursor wait;

	// Now that the PostMessage has arrived, we can reenable the Cancel button
	// without worrying about race conditions.
	CWnd *pCancel = GetParent()->GetDlgItem(IDCANCEL);
	if (pCancel)
		pCancel->EnableWindow(TRUE);

	Task task(this);
	task.Run();

	return 0;
}
```

## Notifying the User

This all works, so far. Unfortunately, even if the task was cancelled, the last page of the wizard still says that it completed successfully. This needs fixing.

The first thing to do is change `ReportComplete` so that it reports the result:

```
inline void Task::ReportComplete(bool bResult)
{
	if (m_pObserver)
		m_pObserver->OnComplete(bResult);
}
```

Obviously, we need to make a bunch of changes to get this to compile. These are in the accompanying source code.
So far, so good: we've got the result of the task back to the progress page. Unfortunately, we need to get it to the last page of the wizard.

We've got a couple of options here: We could have `CProgressPage::OnComplete` save the result before it calls `PressButton(PSBTN_NEXT)`. Then in `OnWizardNext`, it could go to a different page. This might work well. It's unneeded complication in most situations, though.

Another option is to get the result into the final page of the wizard some other way. Something like this (in the constructor of `CProgressWizard`) ought to work:

```
m_bResult = true;

AddPage(new CWelcomePage);
AddPage(new CProgressPage(&m_bResult));
AddPage(new CCompletePage(&m_bResult));
```

The progress page is now free to change the value of `m_bResult` and it'll get communicated successfully to `CCompletePage` without either page knowing about the other. This has the added benefit that, if our task returns some data (other than a true/false result), we can communicate it between the property pages in the same way.
Now we need to change `CCompletePage` so that it notifies the user:

```
BOOL CCompletePage::OnSetActive()
{
	if (!CPropertyPage::OnSetActive())
		return FALSE;

	bool bResult = *m_pbResult;
	if (bResult)
	{
		// Success
		SetDlgItemText(IDC_TITLE, "Wizard completed successfully");
		SetDlgItemText(IDC_MESSAGE,
				"You have successfully completed the wizard."
				"  Press 'Finish' to continue.");

		static_cast<CPropertySheet *>(GetParent())->SetWizardButtons(PSWIZB_FINISH);
	}
	else
	{
		// Failure
		SetDlgItemText(IDC_TITLE, "Wizard failed");
		SetDlgItemText(IDC_MESSAGE,
				"The wizard was not completed successfully."
				"  Press 'Back' to try again,"
				" or 'Finish' to close the wizard.");

		static_cast<CPropertySheet *>(GetParent())->SetWizardButtons(PSWIZB_BACK|PSWIZB_FINISH);
	}

	return TRUE;
}
```

## Conclusions

It works. Unfortunately, because the task runs in the foreground thread, and only periodically reports progress, we only service our message loop intermittently. This means that repaints aren't as quick as they should be; also pressing the Cancel button doesn't respond immediately.

I'll look at these issues in a later article. In the meantime, [here's the source code](/node/view/139).
