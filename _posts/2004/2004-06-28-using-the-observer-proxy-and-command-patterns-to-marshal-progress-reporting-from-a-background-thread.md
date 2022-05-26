---
title: "Using the Observer, Proxy and Command Patterns to Marshal Progress Reporting from a Background Thread"
date: 2004-06-28T12:30:00.000Z
---

In an earlier article, [Running Long-lived Tasks in a Background Thread]({% post_url 2004/2004-02-27-running-long-lived-tasks-in-a-background-thread %}), I talked about some of the problems associated with reporting progress from a background thread in a Win32 application.

In my [Upload Wizard](https://github.com/rlipscombe/WinInetPost) source code, you'll find a better way to implement it, using three of the GoF patterns: the `Observer`, `Proxy` and `Command` patterns.

Here's how it all works.

## The Problem

As explained on the [Running Long-lived Tasks in a Background Thread]({% post_url 2004/2004-02-27-running-long-lived-tasks-in-a-background-thread %}) page, you'll probably use some variant of the Observer pattern, like this:

![[img_assist|nid=188|title=|desc=|link=none|align=center|width=420|height=169]](/broken-image-link-188)

Here the Uploader object makes calls on an UploadObserver interface, which is implemented by the progress dialog (here called CProgressPage).

The code would look a bit like this:

```c++
struct UploadObserver {
    virtual void OnProgress(int num, int denom) = 0;
};

class CProgressPage : public CPropertyPage, public UploadObserver {
    // ...
};

void Uploader::DoUpload()
{
    // ...
    m_pObserver->OnProgress(bytesDone, bytesTotal);
    // ...
}
```

Since the Uploader is running on a background thread, the calls to the UploadObserver interface are also made on the background thread.

A typical response of the CProgressPage object is to update some controls on the dialog. For example, it will call `CProgressCtrl::SetPos`. This resolves into a call to `SendMessage`. Because Windows guarantees that a window procedure will only ever be called from the thread that originally called `CreateWindow`, this call to `SendMessage` will block until the UI thread processes the message.

This should be avoided for (at least) two reasons:

*   The background thread will block until the UI thread processes the message. This might cause a problem if your background thread is doing something that times out.
*   It's now very easy to cause a deadlock. You've just introduced a (hidden) lock dependency between the background thread and the foreground thread.

## PostMessage

Because Windows guarantees that a message will be handled by a window on the thread it was created on, we can easily use this to our advantage.

If the background thread, instead of calling on the observer directly, uses `PostMessage`, the progress updates will arrive (as Windows messages) on the foreground thread correctly.

Unfortunately, this removes our `UploadObserver` object from the picture. The background thread is posting messages, and the progress dialog is handling them directly. The whole point of using an observer was to reduce coupling between the `Uploader` and the `CProgressPage`, which we've now just reintroduced.

## Using a Proxy Window

My preferred solution to this (and I've seen it in other places, too) is to use the Proxy pattern:

![[img_assist|nid=189|title=|desc=|link=none|align=left|width=431|height=147]](/broken-image-link-189)

In this implementation, the `Uploader` object calls methods on the `UploadObserver` interface, as normal. The implementation of `UploadObserver` in `UploadObserverProxy` turns each of these calls into a call to `PostMessage`, sending the messages to itself.

In the relevant message handler, it forwards the call to another `UploadObserver` (the progress dialog). Remember that the initial call is on the background thread, and that the messages must be handled on the foreground thread.

In this way, the proxy ensures that the progress dialog sees the calls on the foreground thread.

This generally results in code that looks a bit like this:

```c++
class CProxyWindow : public CWnd, public UploadObserver {
    UploadObserver *m_pOther;

    // ...
    virtual void OnProgress(int num, int denom)
    {
        PostMessage(MY_WM_PROGRESS, num, denom);
    }

    LRESULT OnMyProgress(WPARAM wParam, LPARAM lParam)
    {
        m_pOther->OnProgress(wParam, lParam);
        return 0;
    }
};
```

The `CProxyWindow::OnProgress` method is called on the background thread, and it uses `PostMessage` to marshal the parameters to the foreground thread.

In the foreground thread, the message is processed by `CProxyWindow::OnMyProgress`, which turns it back into the relevant call to the `UploadObserver` interface.

## Limitations of PostMessage

This approach has some limitations, though:

- We're restricted to passing parameters in `wParam` and `lParam`. In practice, this isn't such a big deal: we can pass
  pointers to structs. This can lead to confusion, though: some methods don't need parameters, some need simple
  integers, some need more complicated things. We also need to be careful about the lifetime of the struct. This
  function will probably have exited by the time the message arrives, meaning that we can't pass an automatic (stack)
  variable.
- We need a message for each progress callback. In practice, we can use the same message and decide what's happening by
  looking at (e.g.) wParam.

## Enter the Command Pattern

The blurb for the Command pattern in the GoF book says this:

> Encapsulate a request as an object, thereby letting you parameterize clients with different requests...

This is essentially what we want. We can change the proxy window to use the command pattern by doing the following:

```c++
struct Command {
    virtual void Execute() = 0;
    virtual ~Command() { /* nothing */ };
};

class OnProgressCommand : public Command {
    UploadObserver *m_pObserver;
    int m_num, m_denom;

public:
    OnProgressCommand(UploadObserver *pObserver, int num, int denom)
        : m_pObserver(pObserver), m_num(num), m_denom(denom)
    {
    }

    virtual void Execute()
    {
        m_pObserver->OnProgress(m_num, m_denom);
    }
};

class CProxyWindow : public CWnd, public UploadObserver {
    UploadObserver *m_pOther;

    // ...
    virtual void OnProgress(int num, int denom)
    {
        Command *pCommand = new OnProgressCommand(m_pOther, num, denom);
        PostMessage(MY_WM_OBSERVER_COMMAND, 0, pCommand);
    }

    LRESULT OnMyObserverCommand(WPARAM wParam, LPARAM lParam)
    {
        Command *pCommand = reinterpret_cast<Command *>(lParam);
        pCommand->Execute();
        delete pCommand;
        return 0;
    }
};
```

Here, we define an abstract class, called `Command`, which defines a method, `Execute`. The various derived classes will override this as appropriate. Note also that (since this is C++) we need to have a virtual destructor in the `Command` class, because we're deleting them through a base-class pointer.

The magic in the proxy now amounts to putting together the relevant command object and posting it to the foreground thread, where it is run (has `Execute` called), and then deleted.

In this way, we provide a simple and consistent way to package up the parameters required by the observer methods. For example, in my upload wizard, the `OnFileProgress` method has 1 string parameter and 7 integer parameters.

In this way, we have successfully combined the `Observer`, `Proxy` and `Command` patterns to provide robust progress reporting from a background thread.

## Pointer-To-Member-Function

There's more, though. Normally a proxy method would look like this:

```c++
void CProxyWindow::OnSomething(int arg1, char *arg2)
{
    class OnSomethingCommand : public Command {
        int m_arg1;
        CString m_arg2;

    public:
        OnSomethingCommand(Progress *pProgress, int arg1, char *arg2)
            : m_pProgress(pProgress), m_arg1(arg1), m_arg2(arg2)
        {
        }

        virtual void Execute() { m_pProgress->OnSomething(m_arg1, m_arg2); }
    };
    Command *commandObject = new OnSomethingCommand(m_pProgress, hr);
    PostMessage(WM_MY_OBSERVER_COMMAND, 0, (LPARAM)commandObject);
}
```

As you can see, using command objects can soon get a little unwieldy. Each observer method on the proxy needs to define its own `Command`-derived object, to encapsulate which method is to be called. We also need to specify each parameter multiple times:

- As parameters to this function.
- As a member variable of the command class.
- As constructor parameters.
- In the initialiser list.
- In the call to the constructor.
- In the actual call inside Execute.

It turns out that, usually, the observer methods will take no arguments, or a single string parameter, or a single integer parameter. We'd therefore expect to be able to handle the three different cases by using a `VoidCommand`, a `StringCommand` and a `IntegerCommand`.

But, of course, how does the command object know which method to call when it arrives at the foreground thread?

Enter one of C++'s more arcane pieces of syntax (at least it was before templates were added to the language): the pointer-to-member-function operator, or `->*` to its friends.

We can define our `VoidCommand` object as follows:

```c++
class VoidCommand : public Command
{
public:
    typedef void (UploadObserver::*OBSERVER_VOID_FUNC) ();

    VoidCommand(UploadObserver *pObserver, OBSERVER_VOID_FUNC pFunc)
        : m_pObserver(pObserver), m_pFunc(pFunc)
    {
    }

    virtual void Execute()
    {
        (m_pObserver->*m_pFunc)();
    }

private:
    UploadObserver *m_pObserver;
    OBSERVER_VOID_FUNC m_pFunc;
};
```

The `typedef` at the top of the class definition defines `OBSERVER_VOID_FUNC` as being a pointer to a method that takes no arguments and returns void, _and that is a member of `UploadObserver`_.

We can use this object as follows:

```c++
Command *commandObject =
        new VoidCommand(m_pProgress,
                &UploadObserver::OnReceivingResponse);
PostMessage(WM_MY_OBSERVER_COMMAND, 0,
        (LPARAM)commandObject);
```

You can see this code in action -- although some of the names are different, and it's been factored a little differently -- in my [File Upload Wizard](https://github.com/rlipscombe/WinInetPost) example.
