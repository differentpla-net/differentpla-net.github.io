---
title: "Synology Router .dss file: What is it?"
date: 2025-05-04T14:19Z
tags: synology-srm
---

I'm considering monkeying around with the firewall rules on my Synology router. If I mess that up, I'll need to
factory-reset the router. So I should probably back up the current configuration first. The _Back Up Configuration_
button downloads a `SynologyRouter_YYYYMMDD.dss` file. What's in it?

tl;dr: it's a `.tar.gz` file containing a SQLite database and a bunch of configuration files.

```
$ file ~/Downloads/SynologyRouter_20250504.dss
~/Downloads/SynologyRouter_20250504.dss: gzip compressed data, last modified: Sun May  4 14:16:37 2025, from Unix, original size modulo 2^32 378880
```

```
$ gunzip < ~/Downloads/SynologyRouter_20250504.dss | tar tf -
ConfigBkp/
ConfigBkp/_Syno_ConfBkp.db
ConfigBkp/config_info
ConfigBkp/srm/
ConfigBkp/srm/usr/
ConfigBkp/srm/usr/syno/
ConfigBkp/srm/usr/syno/etc/
ConfigBkp/srm/usr/syno/etc/synongfw/
ConfigBkp/srm/usr/syno/etc/synongfw/mark_rule.conf
...
```

```
$ mkdir -p tmp; cd tmp
$ gunzip < ~/Downloads/SynologyRouter_20250504.dss | tar xf -

$ file _Syno_ConfBkp.db
_Syno_ConfBkp.db: SQLite 3.x database, last written using SQLite version 3035005, file counter 2, database pages 45, cookie 0x29, schema 4, UTF-8, version-valid-for 2
```

...and so on.
