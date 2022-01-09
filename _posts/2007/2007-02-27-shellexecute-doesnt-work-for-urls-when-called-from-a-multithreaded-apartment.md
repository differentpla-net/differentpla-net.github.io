---
title: "ShellExecute doesn't work for URLs when called from a multithreaded apartment"
date: 2007-02-27T12:14:21.000Z
---
I've got a dialog box with a SysLink (WC_SYSLINK) control on it. When the user clicks on the link, it should open our website in the user's default browser. In my test app, it works. In the production app, it doesn't.

It turns out that the production app calls `CoInitialize(NULL, COINIT_MULTITHREADED)`, and the test app doesn't.

ShellExecute doesn't work from multithreaded apartments. At least, not on Windows XP or 2003\. The same code appears to work fine on Vista.

See the following:

*   [C++ At Work: Making Static Links Keyboard-Capable, Launching URLs from Your App](http://msdn.microsoft.com/msdnmag/issues/05/03/CATWork/default.aspx)
*   [INFO: Calling Shell Functions and Interfaces from a Multithreaded Apartment](http://support.microsoft.com/kb/287087)

If you've not already initialized COM, then it works fine. If you call CoInitialize(NULL), then it works fine. It's only multithreaded apartments that don't work. Paul DiLascia's article (the first link above) has the following workaround if you don't care about the result code:

<pre>LPCTSTR url = _T("www.microsoft.com");
CString args;
args.Format(_T("url.dll,FileProtocolHandler %s"), url);
ShellExecute(NULL, _T("open"), _T("rundll32.exe"), args);</pre>
