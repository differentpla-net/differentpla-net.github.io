---
title: "Running Long-lived Tasks in a Background Thread"
date: 2004-02-27T13:00:00.000Z
x-drupal-nid: 185
x-needs-review: 2004-02-27T13:00:00.000Z
tags: mfc
---
In earlier articles, [Displaying Progress in a Wizard]({% post_url 2004-01-08-displaying-progress-in-a-wizard %}) and [Cancelling Long-Lived Tasks from a Wizard]({% post_url 2004-02-27-cancelling-long-lived-tasks-from-a-wizard %}), I discussed how to run a long-lived task from a wizard and how to display progress in the wizard.

In this article, I'm going to show how to move the long-lived task to a background thread.

At the end of the previous article, I said:

> Unfortunately, because the task runs in the foreground thread, and only periodically reports progress, we only service our message loop intermittently. This means that repaints aren't as quick as they should be; also pressing the Cancel button doesn't respond immediately.

There are a couple of easy fixes to this problem. The first is to make the task report its progress more often. Unfortunately, we often don't have full control over what it's doing, so this option isn't always available.

The second option is to move the task to another thread, leaving the UI thread free to service its message loop.

The first thing we need to do is to change `OnSetActive` so that it starts the task on a background thread:

```
BOOL CProgressPage::OnSetActive()
{
	if (!CPropertyPage::OnSetActive())
		return FALSE;

	// Because the property page isn't fully created and displayed at this point,
	// we need to use PostMessage to defer the long-lasting task for a short while.

	// First, we'll disable the buttons, to prevent the user from
	// pressing anything before we get going.
	static_cast<CPropertySheet *>(GetParent())->SetWizardButtons(0);
	CWnd *pCancel = GetParent()->GetDlgItem(IDCANCEL);
	if (pCancel)
		pCancel->EnableWindow(FALSE);
	m_bCancel = false;

	ASSERT(!m_pThread);
	m_pThread = new TaskThread(this);
	m_pThread->Start();

	return TRUE;
}
```

Because we're going to start the task in the background, we don't need to use `PostMessage` any more, so we can get rid of a bunch of other code.

We also need to implement `TaskThread`:

```
class TaskThread : public Thread
{
	TaskObserver *m_pObserver;

public:
	TaskThread(TaskObserver *pObserver)
		: m_pObserver(pObserver)
	{
	}

	virtual unsigned Run()
	{
		Task task(m_pObserver);
		task.Run();
		return 0;
	}
};
```

class `Thread` is defined in [this page](/node/view/141).

## Threading Issues

As it is, this works (just about). We're not reenabling the Cancel button, and there's a memory leak because we're not deleting the `TaskThread` object.

More seriously, however, we're playing fast and loose with the threading. Because the `Task::Run` function is now being executed on a background thread, all of the progress calls will be run on the background thread as well. Eventually these turn into calls to `SendMessage`, for example `m_progressCtrl.SetPos(current)` calls `SendMessage`.

Windows guarantees that a window procedure will be called on the same thread as the window was created on. When we call `SendMessage` it has to block until the message is processed. This means that the background thread is blocked until the UI thread processes the message. If, while handling that message, we wait for the background thread to do something, we cause a deadlock.

Moreover, when we fix the memory leak as follows:

```
void CProgressPage::OnComplete(bool bResult)
{
	*m_pbResult = bResult;

	// Wait for the thread to finish before deleting it:
	m_pThread->Join();
	delete m_pThread;
	m_pThread = NULL;

	static_cast<CPropertySheet *>(GetParent())->PressButton(PSBTN_NEXT);
}
```

...we'll cause an immediate deadlock, because `OnComplete` is called _on_ the thread that we're attempting to `Join`, so `Join` will never return because the thread's stuck in `Join`!

## PostMessage to the Rescue

Now, since we're not actually bothered that the progress callback be notified immediately, we can use `PostMessage` to decouple the two threads. See, for example, `OnComplete`:

```
void CProgressPage::OnComplete(bool bResult)
{
	PostMessage(MY_WM_COMPLETE, bResult);
}

LRESULT CProgressPage::OnCompleteStub(WPARAM wParam, LPARAM lParam)
{
	bool bResult = wParam ? true : false;

	*m_pbResult = bResult;

	// Wait for the thread to finish before deleting it:
	m_pThread->Join();
	delete m_pThread;
	m_pThread = NULL;

	static_cast<CPropertySheet *>(GetParent())->PressButton(PSBTN_NEXT);

	return 0;
}
```

Now, because of the `PostMessage`, the call to `Join` will happen on the foreground thread, so we've avoided the deadlock.
Note that the parameters that were originally passed to `OnComplete` still need to be passed to `OnCompleteStub`. Here, we package them in `wParam` and `lParam`. If we needed to pass anything more complicated, we could pass a pointer to a (heap-allocated) structure in `lParam`.

Reporting progress is just as simple, except that `OnProgress` is supposed to return `false` to signal cancellation. By the time we receive the posted message, it's too late to return a value. How do we fix this?

It's easy. We just check `m_bCancel` from the background thread:

```
bool CProgressPage::OnProgress(int current, int maximum)
{
	PostMessage(MY_WM_PROGRESS, current, maximum);
	if (m_bCancel)
		return false;	// Stop

	return true;	// Keep going
}

LRESULT CProgressPage::OnProgressStub(WPARAM wParam, LPARAM lParam)
{
	int current = (int)wParam;
	int maximum = (int)lParam;

	m_progressCtrl.SetRange32(0, maximum);
	m_progressCtrl.SetPos(current);

	return 0;
}
```

Is this safe? Yes: `m_bCancel` is only ever set by the UI thread and only ever accessed by the background thread, and it's a boolean, so we're safe. It'll never be in an inconsistent state.

If we were concerned, or we needed something more complicated, we could use a critical section to set and read the value, or one of the `Interlocked` family of functions.

## Enabling the Cancel button

Note that we managed to remove the code that enables the Cancel button. A good way to solve this is to add another method to the observer interface:

```
void CProgressPage::OnBegin()
{
	PostMessage(MY_WM_BEGIN);
}

LRESULT CProgressPage::OnBeginStub(WPARAM wParam, LPARAM lParam)
{
	CWnd *pCancel = GetParent()->GetDlgItem(IDCANCEL);
	if (pCancel)
		pCancel->EnableWindow(TRUE);
	return 0;
}
```

As usual, source code is [here](/node/view/143).
