---
title: "Windows ListView Control Returns -1 From LVM_INSERTITEM"
date: 2004-01-12T12:21:00.000Z
x-drupal-nid: 46
x-needs-review: 2004-01-12T12:21:00.000Z
---
I just spent the best part of a morning chasing this one down, so I thought I'd share it with you:

If you try calling CListCtrl::InsertItem (or LVM_INSERTITEM), using LPSTR_TEXTCALLBACK, you can't use either of the LVS_SORTASCENDING or LVS_SORTDESCENDING styles for the list view.