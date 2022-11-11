---
title: "Installing root CA certificates"
date: 2021-12-21T12:50:00Z
tags: certificates
---

Here's how to install a root CA certificate on various operating systems and platforms.

## Windows

<div class="callout callout-warning" markdown="span">
Note that doing this makes your Windows PC trust certificates issued by this CA, and the script above doesn't restrict
certificate usage. I take no responsibility for any breakage or data loss. **Keep the key secure.**
</div>

1. Double-click on the `k3s-ca.crt` file. Windows will display the certificate properties.
2. Click the "Install Certificate..." button. The Certificate Import Wizard will open.
3. Under "Store Location", select "Local Machine" and click "Next".
4. A UAC prompt will appear; say "Yes".
5. Under "Certificate Store", choose "Place all certificates in the following store" and press the "Browse..." button.
6. Choose the "Trusted Root Certification Authorities" store. Click "OK".
7. Click "Next". Click "Finish".
8. You should see a success message. Click "OK".

## Ubuntu

You need to do the following on every node (and on any other host that needs to push to the registry):

```
sudo mkdir -p /usr/local/share/ca-certificates/differentpla.net
sudo cp k3s-ca.crt /usr/local/share/ca-certificates/differentpla.net/differentpla.net_K3S_CA.crt
sudo update-ca-certificates
```

## Chrome

1. Go to Settings -> Security and Privacy -> Security -> Manage Certificates -> Authorities.
2. Click on "Import"; select the `k3s-ca.crt` file.
3. Ensure that "Trust this certificate for identifying websites" is checked.
4. Click OK.

## Firefox

1. Go to Settings -> Privacy & Security -> Security -> Certificates -> View Certificates -> Authorities.
2. Click on "Import"; select the `k3s-ca.crt` file.
3. Ensure that "Trust this CA to identify web sites" is checked.
4. Click OK.

## Other operating systems and browsers

Left as an exercise for the reader, but I might add them as I need to later.
