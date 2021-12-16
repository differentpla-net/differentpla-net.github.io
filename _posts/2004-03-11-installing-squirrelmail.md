---
title: "Installing SquirrelMail"
date: 2004-03-11T14:22:00.000Z
redirect_from: /node/view/175
tags: qmail
layout: series
series: qmail
---
Part 9 of [Installing qmail and vpopmail](/node/view/165).

## SquirrelMail

One requirement I listed earlier is webmail, so that I can access my email from anywhere. I'm going to take a look at [SquirrelMail](http://www.squirrelmail.org/).

## Configuring Apache

The first thing that I need to do is install Apache (since this is my test box and it's not been installed yet). SquirrelMail also has a bunch of [other requirements](http://www.squirrelmail.org/wiki/SquirrelMailRequirements):

<pre># apt-get install apache php4 php4-pear</pre>

Note that, by default, the Debian packages for php4 on woody don't correctly enable it. You'll need to go and tweak the `/etc/apache/httpd.conf` file:

<pre># LoadModule eaccess_module /usr/lib/apache/1.3/mod_eaccess.so
**LoadModule php4_module /usr/lib/apache/1.3/libphp4.so**
# LoadModule roaming_module /usr/lib/apache/1.3/mod_roaming.so</pre>

<pre>    # And for PHP 4.x, use:
    #
    **AddType application/x-httpd-php .php**
    #AddType application/x-httpd-php-source .phps</pre>

## Installing SquirrelMail

Once Apache and PHP are installed correctly, we can install SquirrelMail. Grab the download from [here](http://www.squirrelmail.org/download.php), and then follow the instructions in the INSTALL file.

The installation instructions aren't great. Here's what I did.

<pre># cd /var/www
# mkdir webmail
# cp -a /usr/local/src/squirrelmail-1.4.2/* webmail/
# cd /var/www/webmail
# chown -R www-data.www-data data</pre>

## Configuring SquirrelMail

The defaults are generally OK, but there's a couple of things that need to be changed:

<pre># config/conf.pl</pre>

`Server Settings`:

<table border="1">
<tbody>
<tr>
<td>Domain</td>

<td>`differentpla.test`</td>

</tr>

</tbody>

</table>

We need to change the settings in `Folder Defaults` to make it compatible with IMAPdir and Outlook Express:

<table border="1">
<tbody>
<tr>
<td>Trash Folder</td>

<td>Trash</td>

</tr>

<tr>
<td>Sent Folder</td>

<td>Sent Items</td>

</tr>

<tr>
<td>Drafts</td>

<td>Drafts</td>

</tr>

</tbody>

</table>

## Configuring .htaccess

In order to allow access as http://flimsy/webmail/, I need to bung a `.htaccess` file in the `/var/www/webmail` directory:

<pre>DirectoryIndex index.php</pre>

And I need to change the `/etc/apache/httpd.conf` file:
<pre><Directory /var/www/webmail/>
        AllowOverride All
</Directory>
</pre>

And it just works. I can log in by going to http://flimsy/webmail/. Cool.

Next: [Installing ClamAV and Qmail-Scanner](/node/view/178).
