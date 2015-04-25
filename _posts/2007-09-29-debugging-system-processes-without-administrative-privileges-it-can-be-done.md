---
title: "Debugging system processes without administrative privileges: it can be done"
date: 2007-09-29T14:14:09.000Z
x-drupal-nid: 196
x-needs-review: 2007-09-29T14:14:09.000Z
---
A short while ago, I asked whether it was possible to [debug system processes without administrative privileges](http://www.differentpla.net/content/2007/08/debugging-system-processes-without-administrative-privileges). I ran into a brick wall and couldn't figure it out.

Today, inspiration strikes: **Use MSVSMON, the remote debugger**:

1.  As Administrator (either through remote desktop, or Fast-User-Switching, run <tt>msvsmon /noauth /anyuser /nosecuritywarn</tt>. It's in <tt>C:\Program Files\Microsoft Visual Studio 8\Common7\IDE\Remote Debugger\x86</tt>. You have to run it with /noauth, because you want to connect from your non-Admin account to the Admin account.
2.  Under Tools/Options remove the timeout setting (clear the edit box), otherwise MSVSMON will keep exiting.
3.  Return to your non-Admin account.
4.  In Visual Studio, bring up the project properties and go to the Debugging page. Set "Debugger to launch" to "Remote Windows Debugger". Set "Remote Command" to $(TargetPath). Set "Connection" to "Remote with no authentication (Native only)"
5.  Set your breakpoints and press F5.

Note that it will punch a hole in the Windows firewall, even if you're connecting to localhost, so don't do this on a network that you don't trust. Not least because it's running with /noauth.