---
title: Securing Synology DSM with a self-signed certificate
date: 2019-06-13 09:05
---

## Create a new self-signed certificate

1. In the DSM Control Panel, navigate to Security / Certificate.
2. Click the 'Add' button.
3. In the wizard, choose 'Add a new certificate'. Click 'Next'.
4. Choose 'Create self-signed certificate'. Click 'Next'.
5. In the 'Create root certificate' page, fill in the details. It wants _all_ the details; I used:
   - Common name: my name
   - Email: my email
   - Location: GB ('cos I'm in London)
   - State/Province: England ('cos the UK doesn't really have states or provinces)
   - City: London ('cos I'm in London)
   - Organization: Home
   - Department: DSM (I guess I could have put 'Hallway Cupboard', but I might move the DiskStation at some point)
6. Click 'Next'.
7. In the 'Create certificate' page:
   - Common name: ds416
   - Subject Alternative Name: ds416;ds416.lan
8. Click 'Apply'.
9. Still in the Certificate tab, click 'Configure'.
10. Set all 3 values to your new certificate. Click 'OK'. Wait while the web server is restarted.

## Export the certificate

**This exports the private keys as well; you might not want to do this.**

1. In the DSM Control Panel, navigate to Security / Certificate.
2. Select the certificate.
3. Click the drop-down next to 'Add'. Click 'Export certificate'.
4. Save the `archive.zip` file somewhere.
5. Extract the `syno-ca-cert.pem` file.

## Teach your PC about the self-signed certificate

This varies by browser and OS. For Firefox 67:

1. Open the Preferences page.
2. Go to 'Privacy & Security'.
3. Scroll down to 'Certificates'.
4. Click 'View Certificates'.
5. On the 'Authorities' tab, click 'Import...'.
6. Choose the `syno-ca-cert.pem` file you exported above.
7. Trust the CA to identify web sites.

Firefox groups certificates by Organization, so mine is listed under 'Home'.

## Done

At this point, you should be able to navigate to `https://ds416.lan:5001`, without any certificate warnings,
and be suitably secured.

Go back to DSM Control Panel / Network / DSM Settings and choose 'Automatically redirect HTTP connections...'.
