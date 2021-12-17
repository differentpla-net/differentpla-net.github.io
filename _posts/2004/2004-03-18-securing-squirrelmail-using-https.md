---
title: "Securing SquirrelMail using HTTPS"
date: 2004-03-18T15:19:00.000Z
redirect_from: /node/view/179
tags: qmail
layout: series
series: qmail
---
Part 11 of [Installing qmail and vpopmail](/node/view/165). This part is about securing webmail access by using HTTPS.

At the end of [Installing SquirrelMail](/node/view/175), I'd finished installing webmail on my test box. Currently, this uses HTTP. This is not really secure enough for webmail, so this article is going to look at adding HTTPS access to webmail.

## Apache-SSL or mod_ssl?

There are two options when it comes to providing support for HTTPS: [Apache-SSL](http://www.apache-ssl.org/) and [mod_ssl](http://www.modssl.org/). Debian has packages for both.

See these two mailing list posts (e.g.) for more information about which to choose:

*   [http://lists.debian.org/debian-isp/2003/debian-isp-200304/msg00260.html](http://lists.debian.org/debian-isp/2003/debian-isp-200304/msg00260.html)*   [http://www.mail-archive.com/modssl-users@modssl.org/msg15791.html](http://www.mail-archive.com/modssl-users@modssl.org/msg15791.html)
    For this box, I'm going to use mod_ssl. My main motivation is that Apache-SSL runs as a separate daemon, which means that I'll have two sets of configuration files to maintain, and a bunch of `apache-ssl` processes using up memory even though this box won't be doing much HTTPS.

    ## Installing mod_ssl

    The first thing to do is to configure Apache to allow SSL access. As you'll recall, my test box is called `flimsy`, so we want to allow access to https://flimsy/. We do this (on Debian) by installing the `libapache-mod-ssl` package:

    <pre># apt-get install libapache-mod-ssl libapache-mod-ssl-doc</pre>

    Information about configuring mod_ssl is in `/usr/share/doc/libapache-mod-ssl-doc` on Debian, but basically, you run `mod-ssl-makecert` to make yourself a certificate for testing, and then tweak your `/etc/apache/httpd.conf` file to turn on SSL. I'll talk about self-signed certificates (and installing them under Windows) later.

    When Apache starts, it'll prompt for the passphrase used to protect the server certificate. See [this](http://bsdvault.net/sections.php?op=viewarticle&artid=85) for one way to work around this. Alternatively, you could just not enter a passphrase.

    With a certificate installed using this method, you'll see a warning like the following:

    [img_assist|nid=211|title=|desc=|link=none|align=left|width=288|height=226] [img_assist|nid=212|title=|desc=|link=none|align=left|width=389|height=278]
    When you create the certificate, you should specify the "Common Name" as the name by which clients will connect to this server. This is generally `www.wherever.com`. If you don't, you'll get a warning: "The name on the security certificate is invalid or does not match the name of the site" or similar.

    For now, if you just hit "Yes", Internet Explorer will use the certificate for this session, allowing you to connect to the server.

    ## Redirecting webmail to HTTPS

    Now, currently, users can connect to the webmail using either HTTP or HTTPS. I'd like to force users to connect using HTTPS. This can be done with some `mod_rewrite` magic.

    To make it go, put a `.htaccess` file in `/var/www/webmail` containing the following:

    <pre>DirectoryIndex index.php

    <IfModule mod_rewrite.c>
            RewriteEngine On
            RewriteCond %{SERVER_PORT}      !^443$
            RewriteRule ^(.*)$      https://%{SERVER_NAME}/webmail/$1 [L,R=303]
    </IfModule></pre>

    If you prefer, you can probably put similar directives in a `<Location>` or `<Directory>` block in `httpd.conf`.
    Note that this stanza is protected by `IfModule`. This causes it to fall back to normal HTTP access if `mod_ssl` didn't load. In such a case, you might prefer to deny access to this directory entirely.

    ## Self-Signed Certificates

    In order to get rid of Internet Explorer's warning about the untrusted certificate, you need to have a signed certificate. For an e-commerce site, this is generally done by giving a stack of cash to [VeriSign](http://www.verisign.com/) or [Thawte](http://www.thawte.com/ssl/index.html). There are other Certificate Authorities, but these are the top two.

    Alternatively, you can create a self-signed certificate and install it in Internet Explorer. For more information about how to do this see, for example, [http://www.modssl.org/docs/2.8/ssl_faq.html#cert-ownca](http://www.modssl.org/docs/2.8/ssl_faq.html#cert-ownca).

    **Note:** If you're planning on using the same server key and certificate with BincIMAP, you'll need to remove the passphrase from the server key. BincIMAP has no way of prompting for the key and, due to the use of tcpserver, couldn't keep it anywhere anyway. For more information, see [http://www.modssl.org/docs/2.8/ssl_faq.html#remove-passphrase](http://www.modssl.org/docs/2.8/ssl_faq.html#remove-passphrase).

    If you follow the instructions given there to create a signed (or self-signed) certificate, you should end up with two files: `server.crt` and `server.key`. If you've installed Debian's `mod_ssl`, you should have directories named `ssl.crt`, `ssl.csr` and `ssl.key` in `/etc/apache`. I just put the files (in this example, I called them `flimsy.key`, `flimsy.csr` and `flimsy.crt` in those directories, and edited `/etc/apache/httpd.conf` to point to them:

    <pre><VirtualHost _default_:443>
         <IfModule mod_ssl.c>
            SSLEngine on
            SSLCertificateFile      /etc/apache/ssl.crt/flimsy.crt
            SSLCertificateKeyFile   /etc/apache/ssl.key/flimsy.key
            SetEnvIf User-Agent ".*MSIE.*" nokeepalive ssl-unclean-shutdown
         </IfModule>
    </VirtualHost></pre>

    ## Installing a Self-Signed Certificate in IE

    If, when presented with the certificate error message, you click on "View Certificate", you'll see your webserver's certificate.

    <table align="center">
    <tbody>
    <tr>
    <td valign="center">[img_assist|nid=213|title=|desc=|link=none|align=left|width=288|height=226]</td>

    <td width="20px"> </td>

    <td valign="center">[img_assist|nid=214|title=|desc=|link=none|align=left|width=307|height=357]</td>

    </tr>

    </tbody>

    </table>

    You'd think that clicking the "Install Certificate" button would suffice, but it doesn't.

    If you're using a self-signed certificate, you need to install your CA certificate in the "Trusted Root Certification Authorities" store. To to this, you can click on the "Certification Path" tab. This will show your server's certificate and your CA certificate. If you click on your CA certificate and click "View Certificate", you can view the details for your root CA certificate.

    To install it, you first need to save it as a file. Select the "Details" tab and click the "Copy to File" button. If you then right-click on this file and select "Install Certificate" you'll see a wizard that will walk you through installing your CA certificate.

    To see it, click on `Tools` / `Options` and go to the "Content" tab. Click on the "Certificates" button. Your certificate will be shown under "Trusted Root Certification Authorities".

    <table align="center">
    <tbody>
    <tr>
    <td valign="center">![](/images/18e4c9a767f8d1947bde086222013223-181.png)</td>

    <td width="20px"> </td>

    <td valign="center">![](/images/52fa0e235abb98bfcb831bd44254b7a9-185.png)</td>

    </tr>

    </tbody>

    </table>

    Now you can point your browser at (in this example) [https://flimsy.home.differentpla.net/webmail/](https://flimsy.home.differentpla.net/webmail/) and it connects without bringing up any warnings.

    For more information about Internet Explorer and certificates, read [Chapter 6 - Digital Certificates](http://www.microsoft.com/resources/documentation/ie/6/all/reskit/en-us/part2/c06ie6rk.mspx) in the Internet Explorer Resource Kit documentation.

    ## Virtual Hosts and HTTPS

    If you're using Virtual Hosts with Apache, you will run into what seems like a major problem: you can't use [name-based virtual hosts](http://httpd.apache.org/docs/vhosts/name-based.html) and HTTPS. For more information, see [http://www.modssl.org/docs/2.8/ssl_faq.html#vhosts](http://www.modssl.org/docs/2.8/ssl_faq.html#vhosts).

    Here are some possible different solutions:

    *   Use [IP-based virtual hosts](http://httpd.apache.org/docs/vhosts/ip-based.html). This requires a different IP address for each virtual host. This can be done by installing more than one network card or by using IP aliasing with a single network card.
         Unfortunately, this won't work in my situation, because I've only got a single IP address for my DSL connection, and I'd need to upgrade my service and buy a router that supported more than one external IP address.
    *   Use a different port for each virtual HTTPS host. This would work in my case, but it's easy to forget to supply the port number when typing in the address.
    *   Don't worry about it. Use the same virtual host to access all of your mailboxes. This works with `vpopmail`, because the domain name is part of the username used to log in. You can do this as long as you don't mind that your users will see that all of your different domains are running off the same server. I don't particularly mind, so this is what I'll do.
    ## Testing with Name-based Virtual Hosts

    `peculiar`, my "production" server is already configured with several name-based virtual hosts. To try this out, I'll have to configure my test box in a similar fashion.

    In [Part 4](/node/view/170), I configured my internal DNS so that both of my test domains would resolve to the same box. If I connect to [http://flimsy.differentpla.test/](http://flimsy.differentpla.test/) or [http://flimsy.beerology.test/](http://flimsy.beerology.test/), I am presented with the same page.

    Configuring name-based virtual hosts to handle this is very easy. Just put something like the following into `/etc/apache/httpd.conf`:

    <pre>NameVirtualHost *

    <VirtualHost *>
        ServerName flimsy.differentpla.test
        DocumentRoot /var/www/flimsy.differentpla.test
    </VirtualHost>

    <VirtualHost *>
        ServerName flimsy.beerology.test
        DocumentRoot /var/www/flimsy.beerology.test
    </VirtualHost></pre>

    This is the minimum needed to get name-based virtual hosts to work. You'll probably want to configure more than just this for each virtual host. For example, `peculiar` has separate log files and custom error documents for each host.

    Any requests that don't match a particular `ServerName` or `ServerAlias` directive will resolve to the first matching `VirtualHost` block.

    We also need a minimal `index.html` in each of the `DocumentRoot` locations, e.g.:

    <pre><!--/var/www/flimsy.beerology.test/index.html-->
    <html>
     <head><title>flimsy.beerology.test</title></head>
     <body>
      <h1>flimsy.beerology.test</h1>
     </body>
    </html>
    </pre>

    We ought also to tweak the `DocumentRoot` settings in the `VirtualHost` block responsible for HTTPS:

    <pre><VirtualHost _default_:443>
        DocumentRoot /var/www/flimsy.home.differentpla.net
        <IfModule mod_ssl.c>
            SSLEngine on
            SSLCertificateFile      /etc/apache/ssl.crt/flimsy.crt
            SSLCertificateKeyFile   /etc/apache/ssl.key/flimsy.key
            SetEnvIf User-Agent ".*MSIE.*" nokeepalive ssl-unclean-shutdown
        </IfModule>
    </VirtualHost>
    </pre>

    If we don't do this, it'll be set to the default (`/var/www` on Debian), which is where each of our virtual hosts is installed. A user will be able to get a list of the virtual hosts on this box by browsing to [http://flimsy.home.differentpla.net/](http://flimsy.home.differentpla.net/). So, we change it.

    Success! Users can connect to either of the name-based virtual hosts, and they can still connect to [https://flimsy.home.differentpla.net/](https://flimsy.home.differentpla.net/webmail/) to check their mail.

    If a user inadvertently tries to connect to [https://flimsy.differentpla.test](https://flimsy.differentpla.test/), which resolves to the same site, they'll be presented with a warning message:

    ![](/images/a94a9bb5b44bf08238a6e8e97cd8a4a4-183.png)

    The user can choose "Yes", and they'll get the webmail login form as normal. In future, we'd probably prefer to use some `mod_rewrite` magic to redirect them to an information page if they've typed in the wrong address. We might also want to use `mod_rewrite` so that people who use HTTPS to connect to pages that don't need to be secure are redirected to use the HTTP variant, thus saving CPU cycles on the server.

    Next: [Securing IMAP](/node/view/190).
