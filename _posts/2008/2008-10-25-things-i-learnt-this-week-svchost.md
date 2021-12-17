---
title: "Things I learnt this week: SVCHOST"
date: 2008-10-25T12:23:02.000Z
x-drupal-nid: 221
x-needs-review: 2008-10-25T12:23:02.000Z
---
SVCHOST services are configured by having ImagePath set to "%windir%\system32\svchost.exe -k _name-of-service_", and a Parameters key containing ServiceDll (REG_EXPAND_SZ), which names a DLL with a ServiceMain entry point. ServiceMain has argc and argv.