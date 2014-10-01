IComHandlerAction
=================

Raymond Chen recently mentioned this, http://blogs.msdn.com/b/oldnewthing/archive/2013/01/04/10382242.aspx, but there doesn't seem to be any decent documentation.

As far as I can work out from drilling through MSDN, you have to do the following. Note that this is pseudo-C++/pseudo-COM:

	ITaskService *pTaskService = CoCreateInstance(CLSID_TaskScheduler);
	pService->Connect();
	ITaskDefinition *pTaskDefinition = pService->NewTask();
	IActionCollation *pActions = pTaskDefinition->get_Actions();
	IComHandlerAction *pAction = (IComHandlerAction *)pActions->Create(TASK_ACTION_COM_HANDLER);
	pAction->ClassId = CLSID_MyAction;
	
	// etc.

	pTaskFolder->RegisterTaskDefinition(pTaskDefinition);

There's a simple C++ example here: http://msdn.microsoft.com/en-us/library/windows/desktop/aa383624(v=vs.85).aspx
