---
title: "Displaying Progress in a Wizard"
date: 2004-01-08T18:24:00.000Z
---
I'm adding a wizard to the program that I'm currently working on. The wizard walks the user through importing some information from a file. I'd like to be able to display the import progress as a seamless part of the wizard.

The import process is long-winded, and the details don't particularly matter here, so I'll fake it by using the following:

<pre>void Task::Run()
{
	// Pretend to do some work.
	const int MAX_ITERATIONS = 10;
	for (int i = 0; i < MAX_ITERATIONS; ++i)
	{
		DWORD startTicks = GetTickCount();
		ReportProgress(i, MAX_ITERATIONS);
		Sleep(1000);
	}

	ReportProgress(MAX_ITERATIONS, MAX_ITERATIONS);
	ReportComplete();
}</pre>

The `ReportProgress` and `ReportComplete` methods are standard observer stuff. They end up calling the following methods:

<pre>void CProgressPage::OnProgress(int current, int maximum)
{
	PumpMessages();

	m_progressCtrl.SetRange32(0, maximum);
	m_progressCtrl.SetPos(current);
}

void CProgressPage::OnComplete()
{
	PumpMessages();

	static_cast<CPropertySheet *>(GetParent())->PressButton(PSBTN_NEXT);
}</pre>

Initially, I tried to get this to work from `OnSetActive`, but it doesn't work. The property page is never displayed. I'd assumed that the `PumpMessages` call would allow the repaints to occur. Worse, because the handler for `OnComplete` calls `PressButton(PSBTN_NEXT)` while in the `OnSetActive` method, it gets called again, and again, and again,...

<pre>// This doesn't work...
BOOL CProgressPage::OnSetActive()
{
	if (!CPropertyPage::OnSetActive())
		return FALSE;

	// Disable the buttons.
	static_cast<CPropertySheet *>(GetParent())->SetWizardButtons(0);
	CWaitCursor wait;

	Task task(this);
	task.Run();

	return TRUE;
}</pre>

Luckily, it works OK if you call `PostMessage` to return from `OnSetActive` and then handle it later:

<pre>BOOL CProgressPage::OnSetActive()
{
	if (!CPropertyPage::OnSetActive())
		return FALSE;

	// Disable the buttons.
	static_cast<CPropertySheet *>(GetParent())->SetWizardButtons(0);
	PostMessage(WM_START_TASK);

	return TRUE;
}

LRESULT CProgressPage::OnStartTask(WPARAM wParam, LPARAM lParam)
{
	// Start doing whatever it is that we have to do.
	CWaitCursor wait;

	Task task(this);
	task.Run();

	return 0L;
}</pre>

[img_assist|nid=41|title=|desc=|link=none|align=left|width=640|height=496]

I still need to implement the Cancel button, and there's other things I should say about running long processes in the foreground thread, but I'll save that for another article.

Source code for this article is attached.
