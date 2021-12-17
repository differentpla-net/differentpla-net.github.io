---
title: "Setting Internet Explorer and Windows Update to use Squid"
date: 2009-09-16T08:48:05.000Z
x-drupal-nid: 240
x-needs-review: 2009-09-16T08:48:05.000Z
---
If you've [installed squid]({% post_url 2009/2009-09-16-setting-up-squid-on-windows %}) on a box, you'll probably want to use it.

## Internet Explorer

To set up Internet Explorer to use it, go to **Tools** / **Internet Options**, then go to the **Connections** tab. Click on the LAN settings button. In the dialog that appears, set up **Proxy server** as follows:

1.  Check the "Use a proxy server for your LAN..." box.
2.  Put the host name or IP address of your Squid proxy in the "Address" field.
3.  The default port number for Squid is **3128**, so put this in the "Port" field.
4.  Check the "Bypass proxy server for local addresses" box.

## Windows Update

Configuring Internet Explorer sets the WinInet proxy configuration settings. Windows Update uses the WinHttp proxy configuration settings. You need to copy the WinInet settings to the WinHttp settings. Fortunately, it's pretty easy.

### Windows XP

<pre>proxycfg -i</pre>

### Windows Vista

<pre>netsh winhttp import proxy source=ie</pre>
