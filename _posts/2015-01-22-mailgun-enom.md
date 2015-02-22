---
title: Configuring Mailgun DNS entries on eNom
date: 2015-02-22 09:14
---

When setting up a Mailgun domain, you need to configure some DNS entries, as follows:

    TXT     mg.example.com                  v=spf1 include:mailgun.org ~all
    TXT     pic._domainkey.mg.example.com   k=rsa; ...

    CNAME   email.mg.example.com            mailgun.org

With eNom, you need to omit the `.example.com`, so:

    mg                  TXT         v=spf1 ...
    pic._domainkey.mg   TXT         k=rsa; ...
    email.mg            CNAME       mailgun.org.

Also note the trailing `.` on the CNAME entry.
