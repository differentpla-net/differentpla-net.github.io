---
title: The case of the missing print jobs
date: 2013-04-27T08:02:22Z
---
Having recently moved my home server (Windows 2012) from the hall cupboard to the corner of my study, I decided to connect it to my printer. This means that I can print from my laptop over wireless without needing to turn on the desktop PC that the printer was originally connected to.

It didn't work: Windows correctly installed the printer drivers, and added the printer as the default device, but whenever I attempted to print anything (even locally), nothing came out, but no error messages were displayed.

In order to track down the problem, I tried printing using the XPS driver. That didn't work either: the print job was reported as successful, but I ended up with zero-byte `.OXPS` files.

Eventually, I looked in *Event Viewer* and found this:

    Log Name:      Microsoft-Windows-PrintService/Admin
	Source:        Microsoft-Windows-PrintService
	Date:          15/04/2013 19:06:39
	Event ID:      372
	Task Category: Printing a document
	Level:         Error
	Keywords:      Classic Spooler Event,Document Print Job
	User:          HOME\Administrator
	Computer:      SERVER.home.differentpla.net
	Description:
	The document Print Document, owned by Administrator, failed to print on printer
	Microsoft XPS Document Writer. Try to print the document again or restart the print spooler. 
	Data type: RAW. Size of the spool file in bytes: 67709. Number of bytes printed: 0.
	Total number of pages in the document: 2. Number of pages printed: 0.
	Client computer: \\SERVER. Win32 error code returned by the print processor: 5. Access is denied.

I searched the Internet for this particular Event ID, and for the error message. Almost everything was talking about printing from Terminal Services, which was not my problem, because I was trying to print locally.

In the end, I found a [KB article](http://support.microsoft.com/kb/962930),
which suggests giving Everyone permission on the `PRINTERS` folder.

There's no way that I'm doing that, so I looked around some more, and [found something](http://answers.microsoft.com/en-us/windows/forum/windows_7-hardware/win32-error-code-returned-by-the-print-processor-5/df308c63-c654-e011-8dfc-68b599b31bf5) that mentioned using *Process Monitor* and looking at the `spoolsv.exe` process.

That didn't show anything interesting, but it did lead me to filtering on the `PRINTERS` folder. Instead of `spoolsv.exe`, I found a process named `printfilterpipelinesvc.exe`, which was getting **ACCESS DENIED** while attempting to create a `.TMP` file in the `PRINTERS` folder.
Looking at the properties for that entry, we find that `printfilterpipelinesvc.exe` is running as `LOCAL SERVICE`.

So, I just gave `LOCAL SERVICE` full control over the `PRINTERS` folder, and everything started working.
