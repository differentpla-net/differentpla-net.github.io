---
title: "Resource File Dependencies"
date: 2004-01-10T21:49:22.000Z
tags: jam
---
Our MFC application has a resource script. This resource script suffers from a minor problem: It's not dependency-scanned. If we edit any file included by it -- for example the `.rc2` file, it's not rebuilt properly.

We need to add the following to our `Resource` rule:

```
    NEEDLIBS on $(_e) += $(_r) ;

    # .rc files have #includes, but this limits the dependency search to
    # the .rc's directory and the SubDirHdrs for this directory.

    HDRS on $(_r) = $(HDRS) $(SEARCH_SOURCE) $(SUBDIRHDRS) ;

    HDRRULE on $(_s) = HdrRule ;
    HDRSCAN on $(_s) = $(HDRPATTERN) ;
    HDRSEARCH on $(_s) = $(SEARCH_SOURCE) $(SUBDIRHDRS) ;
    HDRGRIST on $(_s) = $(HDRGRIST) ;

    Rc $(_r) : $(_s) ;
```

Source is [here](/files/jam-test-20010717a.tar.gz).
