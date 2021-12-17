---
title: "PowerShell tip: Deleting certificates"
date: 2012-07-31T10:20:48.000Z
x-drupal-nid: 280
x-needs-review: 2012-07-31T10:20:48.000Z
---
    $certs = Get-ChildItem cert:\LocalMachine\My | where { $_.Subject –like 'CN=Victim*' }
    foreach ($cert in $certs) {
        $store = Get-Item $cert.PSParentPath
        $store.Open('ReadWrite')
        $store.Remove($cert)
        $store.Close()
    }

The first line finds certificates with the matching subject. The loop goes through those certificates, using .NET class library methods to delete them.

The clever bit is that `Get-Item $cert.PSParentPath` returns the `X509Store` object that is the certificate's store.
