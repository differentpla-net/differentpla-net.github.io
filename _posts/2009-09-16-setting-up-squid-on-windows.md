---
title: "Setting up Squid on Windows"
date: 2009-09-16T08:38:27.000Z
x-drupal-nid: 239
x-needs-review: 2009-09-16T08:38:27.000Z
---
At work, we do a lot of our testing and debugging with virtual machines (using Microsoft Virtual Server or Hyper-V, mostly). The virtual guests are generally not allowed any access to our live network, which makes accessing the Internet difficult. To get around this, I usually set up Squid on the host machine. This allows the guests to access (e.g.) Windows Update and Microsoft Symbol Server, which makes things a bit easier.

Here's my quick guide to setting up Squid on Windows.

1.  Download the latest stable version of Squid from [http://squid.acmeconsulting.it/download/dl-squid.html](http://squid.acmeconsulting.it/download/dl-squid.html). At the time of writing this is [Squid 2.7-stable6](http://squid.acmeconsulting.it/download/squid-2.7.STABLE6-bin.zip).
2.  Unzip it to <tt>C:\</tt>. You should end up with a directory called <tt>C:\squid</tt>.
3.  In the <tt>C:\squid\etc</tt> directory, rename the configuration files as follows:
    *   mime.conf.default to mime.conf
    *   squid.conf.default to squid.conf
4.  If you want to put the cache and log directories on another drive -- they get quite large, edit the squid.conf file (see below).
5.  Run c:\squid\bin\squid -i to install it as a Windows service. You'll need to be elevated at this point.
6.  Run c:\squid\bin\squid -z to initialize the cache directories.
7.  Open up your firewall to either allow squid.exe or port 3128
8.  Start the squid service

## Using a different drive for the cache and log directories

<pre>--- squid.conf.default  2008-10-21 00:03:06.000000000 +0100
+++ squid.conf  2009-01-06 10:03:48.160910000 +0000
@@ -1928,7 +1928,7 @@
 #      (hard coded at 1 MB).
 #
 #Default:
-# cache_dir ufs c:/squid/var/cache 100 16 256
+cache_dir ufs e:/squid/var/cache 100 16 256

 #  TAG: store_dir_select_algorithm
 #      Set this to 'round-robin' as an alternative.
@@ -2100,7 +2100,7 @@
 #
 #      And priority could be any of:
 #      err, warning, notice, info, debug.
-access_log c:/squid/var/logs/access.log squid
+access_log e:/squid/var/logs/access.log squid

 #  TAG: log_access     allow|deny acl acl...
 #      This options allows you to control which requests gets logged
@@ -2123,7 +2123,7 @@
 #      logged to this file with the "debug_options" tag below.
 #
 #Default:
-# cache_log c:/squid/var/logs/cache.log
+cache_log e:/squid/var/logs/cache.log

 #  TAG: cache_store_log
 #      Logs the activities of the storage manager.  Shows which
@@ -2133,7 +2133,7 @@
 #      disable it.
 #
 #Default:
-# cache_store_log c:/squid/var/logs/store.log
+cache_store_log e:/squid/var/logs/store.log

 #  TAG: cache_swap_state
 #      Location for the cache "swap.state" file. This index file holds
@@ -4872,7 +4872,7 @@
 # coredump_dir none
 #
 # Leave coredumps in the first cache dir
-coredump_dir c:/squid/var/cache
+coredump_dir e:/squid/var/cache

 #  TAG: chroot
 #      Use this to have Squid do a chroot() while initializing.  This
</pre>