---
title: "Shell_NotifyIcon: Displaying a balloon from a hidden notification icon"
date: 2007-10-01T08:05:56.000Z
---
You should hide your notification icon unless you've got something important to tell the user. More and more applications insist on putting mysterious icons in the notification area, and most users have no idea what they're for.

The clue's in the name: "notification icon". They're used for notification. For configuration options for your program, create a Control Panel applet, or add a property page to somewhere sensible.

When a notification icon is hidden, it cannot bring up balloon notifications, so you'll need to show it first and hide it later.

You can hide a notification icon like this:

<pre>nid.uFlags |= NIF_STATE;
nid.dwState = NIS_HIDDEN;
nid.dwStateMask = NIS_HIDDEN;</pre>

...either when sending NIM_ADD, or by using NIM_MODIFY.
You can show the icon like this:

<pre>nid.uFlags |= NIF_STATE;
nid.dwState = 0;
nid.dwStateMask = NIS_HIDDEN;</pre>

...as part of NIM_MODIFY.
This only works on Shell and Common Controls version 5.0 (Windows 2000 or Internet Explorer 5.0) or newer. On earlier versions of Windows, you'll need to use NIM_DELETE and NIM_ADD as appropriate.

You can show a balloon by using the NIF_INFO flag with NIM_MODIFY:

<pre>nid.uFlags = NIF_INFO;
nid.dwInfoFlags = NIIF_INFO;
_tcscpy_s(nid.szInfoTitle, _T("Title"));
_tcscpy_s(nid.szInfo, _T("Some text goes here"));
</pre>

So, you should show your icon (and balloon) when you need to tell the user something, and then hide it again in response to the NIN_BALLOONHIDE, NIN_BALLOONTIMEOUT and NIN_BALLOONUSERCLICK notifications.