---
title: "Win32 Threads in C++"
date: 2003-04-15T10:20:00.000Z
---
<div class="snippet">
    /* thread.cpp
     */

    #define WIN32_LEAN_AND_MEAN
    #include <windows.h>
    #include <process.h>

    class Thread
    {
        HANDLE m_hThread;
        UINT m_idThread;

    public:
        Thread()
    	: m_hThread(NULL), m_idThread(0)
        {
        }

        virtual ~Thread()
        {
    	CloseHandle(m_hThread);
        }

        bool Start()
        {
    	unsigned threadId;
    	HANDLE hThread = (HANDLE)_beginthreadex(NULL, 0, StaticThreadRoutine,
    					this, CREATE_SUSPENDED, &threadId);
    	if (!hThread)
    	    return false;

    	m_hThread = hThread;
    	m_idThread = threadId;
    	ResumeThread(hThread);

    	return true;
        }

        unsigned Join()
        {
    	WaitForSingleObject(m_hThread, INFINITE);

    	DWORD result;
    	GetExitCodeThread(m_hThread, &result);

    	return result;
        }

    protected:
        virtual unsigned Run() = 0;

    private:
        static unsigned __stdcall StaticThreadRoutine(void *pParams)
        {
    	return static_cast<Thread *>(pParams)->Run();
        }
    };

    class MyThread : public Thread
    {
    protected:
        virtual unsigned Run()
        {
    	Sleep(1000);

    	return 0;
        }
    };

    int main(void)
    {
        MyThread *myThread = new MyThread;
        myThread->Start();

        myThread->Join();
        delete myThread;

        return 0;
    }

</div>