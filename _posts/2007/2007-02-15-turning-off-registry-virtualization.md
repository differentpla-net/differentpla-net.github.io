---
title: "Turning off Registry Virtualization"
date: 2007-02-15T10:51:58.000Z
x-drupal-nid: 97
x-needs-review: 2007-02-15T10:51:58.000Z
---
On Windows Vista, applications running without admin privileges can write to HKEY_LOCAL_MACHINE, and they'll be transparently redirected to HKEY_CURRENT_USER\Software\Classes\VirtualStore. Similar redirection applies to C:\Program Files.

See [Jerry's Incoherent Babbling](http://windowsconnected.com/blogs/jerry/archive/2005/12/19/86.aspx) for more details.

I spent a couple of hours looking for a way to turn this off or detect it programmatically. There doesn't appear to be an API call for this.

What I was missing was a single sentence on [this MSDN page](http://msdn2.microsoft.com/en-us/library/aa965884.aspx). If you have an application manifest file that specifies the requestedExecutionLevel attribute, registry virtualization is turned off for that application.

I guess your manifest is saying "Yeah, I know about the new Vista UAC stuff; I don't need the backward compatibility hack, thanks."