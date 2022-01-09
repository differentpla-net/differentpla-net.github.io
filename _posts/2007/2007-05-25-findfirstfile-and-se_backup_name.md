---
title: "FindFirstFile and SE_BACKUP_NAME"
date: 2007-05-25T17:04:57.000Z
---
If you want to read a file and bypass security (e.g., if you're writing a backup program), it's not enough to simply enable the SE_BACKUP_NAME privilege, you have to also pass FILE_FLAG_BACKUP_SEMANTICS to CreateFile.

Fortunately, you don't have to do this for FindFirstFile. Enabling the privilege is enough.
