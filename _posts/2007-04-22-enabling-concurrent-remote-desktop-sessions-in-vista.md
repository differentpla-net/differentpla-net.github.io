---
title: "Enabling Concurrent Remote Desktop Sessions in Vista"
date: 2007-04-22T20:26:08.000Z
x-drupal-nid: 165
x-needs-review: 2007-04-22T20:26:08.000Z
---
At work, I do all of my development on a Windows 2003 box. I generally log in as a normal user, and then use Remote Desktop to concurrently log in as Administrator. This means that I'm relatively safe from malware when doing day-to-day stuff, but that I can still get hold of Administrator privileges when I need them.

This feature is only enabled on server editions of Windows. When you try to connect to Windows Vista (for example), you have to disconnect the currently logged-on user.

This is taken more-or-less, directly from [here](http://www.missingremote.com/index.php?option=com_content&task=view&id=1220&Itemid=1), so I can't take any credit.

You'll need a hacked termsrv.dll, as follows:

<pre>fc /b %SYSTEMROOT%\System32\termsrv.dll termsrv_new.dll
Comparing files C:\WINDOWS\SYSTEM32\termsrv.dll and TERMSRV_NEW.DLL
000150D8: 3B BA
000150D9: 91 00
000150DA: 20 01
000150DB: 03 00
000150DD: 00 90
000150DF: 0F 89
000150E0: 84 91
000150E1: 0C 20
000150E2: CA 03
000154BF: 43 90</pre>

(This one's for Windows Vista Ultimate, x86 (i.e. 32-bit).
And then you'll need to install it, as follows:

<pre>cd /d %SYSTEMROOT%\System32
sc stop UmRdpService
sc stop TermService
takeown /f termsrv.dll
cacls termsrv.dll /G DOMAIN\User:F
ren termsrv.dll termsrv.dll.orig
copy path\to\termsrv_new.dll termsrv.dll
sc start TermService
sc start UmRdpService</pre>

Done.

Note that if you want to connect to localhost, you'll need to run some kind of port forwarding utility, and you'll need to connect to it via 127.0.0.2.