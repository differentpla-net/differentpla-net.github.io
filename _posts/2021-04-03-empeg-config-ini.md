---
title: "Editing empeg config.ini"
date: 2021-04-03T16:08:00Z
tags: empeg
---

Editing the empeg's config.ini file with FTP.

```
$ ncftp ${EMPEG_IP}
ncftp / > site rw
ncftp / > cd empeg/var
ncftp /empeg/var > get config.ini
```

...edit the file.

```
ncftp /empeg/var > put -f config.ini
ncftp /empeg/var > site ro
ncftp /empeg/var > site reboot
```
