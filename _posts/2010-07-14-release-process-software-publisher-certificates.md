---
title: "Release Process: Software Publisher Certificates"
date: 2010-07-14T13:35:11.000Z
x-drupal-nid: 261
x-needs-review: 2010-07-14T13:35:11.000Z
---
Since your code signing certificate probably lasts for two years, you’ll have forgotten what to do with it when you receive your new one.

If you’re using [Verisign](http://www.verisign.com/), they’ll send you the certificate and key as two separate files named AcmeCertificate.spc and AcmePrivateKey.pvk (or similar).

In order to use these for code signing without being prompted for the password each time, you’ll need to convert the two files into a PFX file and import it into the Windows certificate store. Think about [key security](/content/2010/05/release-process-code-signing-key-security) before you do this, though.

To do this, use pvk2pfx, as follows:

pvk2pfx –pvk AcmePrivateKey.pvk –spc AcmeCertificate.spc –pfx AcmeCertificateAndKey.pfx

This will prompt you for the passphrase for the private key. It will create a .PFX file without a passphrase. If you want a passphrase on the PFX file (and you probably do), you should use the –po switch.

Then you can [import the PFX file](http://msdn.microsoft.com/en-us/library/ff546307(VS.85).aspx) by using CERTUTIL, as follows:

certutil –user –importPFX AcmeCertificateAndKey.pfx

This stores the certificate and key in the current user's Personal certificate store, where it can be used for signing.

To use it for signing, use signtool, as follows:

signtool sign /sha1 certificate-thumbprint /d Description /du Url /t http://timestamp.verisign.com/scripts/timstamp.dll Path\To\Application.exe

TODO: Generating your own, test, certificates and keys.
