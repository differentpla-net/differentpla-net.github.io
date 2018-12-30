---
title: "Error: Failed to generate a user instance of SQL Server..."
date: 2006-01-18T21:55:30.000Z
x-drupal-nid: 2
x-needs-review: 2006-01-18T21:55:30.000Z
---
I was just hacking something together in Visual Web Developer Express, and was attempting to add a database to my App_Data directory when I ran into problems.

VWD kept timing out, and giving me a variety of error messages. Event Viewer contained the following: `Failed to generate a user instance of SQL Server due to a failure in starting the process for the user instance. The connection will be closed. [CLIENT: <local machine="">]</local>`

I did some googling, and discovered the solution: you need to delete the `C:\Documents and Settings\_username_\Local Settings\Application Data\Microsoft\Microsoft SQL Server Data\SQLEXPRESS` directory.

What everyone else failed to emphasise is that you might need to **restart Windows**. Simply bouncing the SQL Server Express service doesn't always fix it.

This is what I did:

*   Reboot the computer.
*   Log in as Administrator (I run with non-Admin privileges most of the time).
*   Delete the SQLEXPRESS directory for my normal user account.
*   Log back in as my normal user.

You can see if it's working properly by using the [SSEUTIL](http://www.microsoft.com/downloads/details.aspx?FamilyID=fa87e828-173f-472e-a85c-27ed01cf6b02&DisplayLang=en) utility. "sseutil -l" (that's a lower-case L) should list the databases currently being used by your user account.
