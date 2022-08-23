---
title: "SqlException: Login failed for user 'IIS APPPOOL\\ASP.NET v4.0'"
date: 2011-01-30T13:55:19.000Z
tags: wcf
---
I’m spiking a WCF service hosted in IIS this weekend. It uses SQL Server, and I was getting the following error in WCF Tracing:

    Login failed for user 'IIS APPPOOL\ASP.NET v4.0'.

Fortunately, I quickly found [this thread on the ASP.NET forums](http://forums.asp.net/t/1510479.aspx). To summarise:

*   On IIS 7 and later, _don’t_ change it to use “NetworkService”.
*   In SQL Server, under Security / Logins, create the username exactly as displayed. You can’t search for it.
*   In SQL Server, under _database_ / Security / Users, add the login as a user on your new database.

For the last part, you can add the login to the db_owner role during development, but you really ought to lock the permissions down properly (give SELECT / INSERT / etc. permissions as appropriate).
