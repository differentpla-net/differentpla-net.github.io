---
title: "Lazy Bastard's Guide to Cloning your empeg"
date: 2002-03-18T07:59:00.000Z
redirect_from: /node/view/111
tags: empeg
---
## Problem/Context

* You've taken advantage of the recent "fire sale" at [SONICblue](http://www.sonicblue.com/) to acquire a shiny new empeg. You'd like to clone your existing music collection to it.
* You didn't keep the tags up-to-date on your local PC.
* You don't have enough disk space to download the contents of your empeg to your local PC -- or you do, but can't be bothered.
* You can't get `empegClone` to work -- your serial numbers are the wrong way round perhaps.
* You can't be bothered to install `hijack` to get an ftp server -- besides, you can't remember how to get `ftp` to not ask you for every file when using `mget`, and installing `ncftp` on your empeg is a faff.
* You've got a Linux box, or a copy of cygwin.
* Your new empeg has the same number of disks in it as your old empeg, and they're the same size or larger. Without this, it gets complicated.
* You're a lazy bastard, like me.

## Before You Start

### Run fsck on the player

Before you start, it's probably worth running fsck on both the source and destination players. This is to ensure that the disks are in a consistent state. To do this, check the instructions in the [FAQ](http://www.riocar.org/modules.php?op=modload&name=FAQ&file=index&myfaq=yes&id_cat=8&categories=Known+problems+and+troubleshooting+questions#162) at [riocar.org](http://www.riocar.org/).

## Solution

### Download the ARM binary of netcat

Go along to [http://ftp.us.debian.org/debian/dists/stable/main/binary-arm/net/](http://ftp.us.debian.org/debian/dists/stable/main/binary-arm/net/), (or your local mirror) and grab a copy of [netcat_1.10-12.1.deb](http://ftp.us.debian.org/debian/dists/stable/main/binary-arm/net/netcat_1.10-12.1.deb).

### Install netcat

1.  `.deb` files are just `ar` archives, containing a couple of `.tar.gz` files, and a tag file. You need to get the "data.tar.gz" file. Do this:

    ```
    not-the-empeg:~$ ar p netcat_1.10-12.1.deb data.tar.gz > netcat_1.10-12.1.tar.gz
    ```

    This extracts the `data.tar.gz` file and saves it under a sensible name.

2.  Transfer the file that you created above (`netcat_1.10-12.1.tar.gz`) using your favourite serial terminal's ZModem command to `/drive0`, then extract the files:

    ```
    empeg:/# rwm ; rw
    empeg:/# cd /drive0
    ```

    (Transfer the file using ZModem)
    ```
    empeg:/drive0# cd /
    empeg:/# tar xvfz drive0/netcat_1.10-12.1.tar.gz
    ```

    You should see the some files being extracted from the .tar.gz file.

3.  Repeat, but for the other empeg.

**Note:** For the Linux-challenged amongst you, I've put the netcat_1.10-12.1-minimal.tar.gz file [here](/node/view/227). Copy it to the empeg in step 2, above, changing the 'tar xvfz' command to suit.

### Transfer the data files

1.  Run netcat in "listen mode" on the destination (new) empeg:

    ```
    dest-empeg:/# cd /drive0
    dest-empeg:/drive0# rwm
    dest-empeg:/drive0# nc -l -p 2999 | tar xvf -
    ```

    This runs netcat in listen mode, on port 2999, and then sends any output it has through tar, to be extracted.

2.  Tar up the files on the source (old) empeg, and send the output to netcat:

    ```
    source-empeg:/# cd /drive0
    source-empeg:/drive0# tar cvf - fids foo | nc dest-empeg 2999
    ```

    This archives the files in `/drive0/fids` and uses netcat to send them to the other empeg. Because netcat only exits at end-of-file, we must press Ctrl+C to stop it. So that we know when this is, we add a spurious 'foo' to the list of files to be transferred -- when tar complains, we're finished.

    While this process is going on (it'll take a long time), you should see the filenames go past, like this:

    ```
    fids/100
    fids/101
    fids/110
    fids/111
    ```

    If you see anything markedly different, you've done something wrong.

    Wait until it's finished (tar complains about "foo". Press Ctrl+C on both empegs to stop the transfer. Check that the files were transferred correctly:

    ```
    dest-empeg:/drive0# ls fids
    ```

    You should see a lot of filenames whizz past.

3.  Repeat, but for `/drive1`

### Rebuild the database

1.  On the destination (new) empeg, delete the existing database:

    ```
    dest-empeg:/# cd /empeg/var
    dest-empeg:/empeg/var# rwm
    dest-empeg:/empeg/var# rm tags database playlists
    ```

2.  Restart the player:

    ```
    dest-empeg:/empeg/var# exit
    ```

    Note that we _don't_ remount the disks read-only. This allows the player to save the database once it has rebuilt it. In order to ensure that the disks are mounted read-only before you yank the power, exit the player (using 'q'), or run emplode against it.

3.  Run emplode against both players. It will rebuild the database.

## Resulting Context

A properly cloned player. Almost. The dynamic data partition (EQ, bookmarks, etc.) isn't cloned. This should be easy enough, but I've got nothing on there that I'm bothered about, so I didn't bother.

How fast is it? Approximately 1Gb/hour. I cloned 32Gb, it took a little over 32 hours. However, I did this with the 'z' option to tar. I've updated the page to remove this, because it's apparently faster without.

## More Information

You might also want to check out my page on [using rsync to clone your empeg]({% post_url 2002-03-07-using-rsync-to-synchronise-empegs %}).
