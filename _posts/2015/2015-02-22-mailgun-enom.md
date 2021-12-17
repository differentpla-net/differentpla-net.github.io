---
title: Configuring Mailgun DNS entries on eNom
date: 2015-02-22 09:14
---

When setting up a Mailgun domain, you need to configure some DNS entries, shown
as follows in the Mailgun console:

    TXT     mg.example.com                  v=spf1 include:mailgun.org ~all
    TXT     pic._domainkey.mg.example.com   k=rsa; ...

    CNAME   email.mg.example.com            mailgun.org

With eNom, you need to omit the `.example.com`, and use a trailing `.` on the
CNAME, so add entries as follows:

    mg                  TXT         v=spf1 ...
    pic._domainkey.mg   TXT         k=rsa; ...
    email.mg            CNAME       mailgun.org.
