---
title: "Debugging system processes without administrative privileges: can it be done?"
date: 2007-08-13T10:17:36.000Z
x-drupal-nid: 192
x-needs-review: 2007-08-13T10:17:36.000Z
---
I've been running with a normal, non-administrator (LUA) account for the past 2 years or so, without any real problems. Most of the time, the problem is simply that you can't install something, or register something, or you need to install a service.

In most of these cases, you can simply switch to an administrative session (using local remote desktop or fast user switching), do what you need to do, and then switch back.

When debugging things, you can usually get away with running them under your user account, which is fairly easily accomplished.

Sometimes, however, you want to run a Windows service running as LocalSystem, which becomes quite difficult.

I _could_ give my user account the SE_DEBUG_NAME privilege, but this allows unfettered access to the memory of any process running on the computer. This is as good as having administrative privileges to any malware that knows how to take advantage of it.

I've looking for something more focussed, where I can give myself permission to debug specific service processes.

If you try attaching to the process from Visual Studio 2005, you get the message `Unable to attach to the process. You do not have permission to debug the process.`

Using this as a search term mostly brings up people having problems with Windows Vista and UAC. I'm running on Windows 2003, so this isn't my problem. My problem is that, simply, I _don't_ have permission to debug the process.

My first attempt to work around this (which didn't work) was to tweak the permissions of the process, by using SysInternals Process Explorer:

1.  In the Process tree view, bring up the properties for the relevant process.
2.  On the Security tab, you can see what account the process is running under, which groups it belongs to, and which privileges it has, showing whether they're enabled or disabled. This is interesting, but you need to click on the Permissions button.
3.  You can add your user account, granting Full Control, using this dialog box.

This time, we still can't attach the debugger, but we get a different error message: `Unable to attach to the process. You do not have permission to inspect the process's identity`. We're getting closer, but still no cigar.

Unfortunately, searching for this message talks about remote debugging and Terminal Services, both of which are only vaguely related to what I'm trying to do here.

And that's where I run into a brick wall. Is this even possible?
