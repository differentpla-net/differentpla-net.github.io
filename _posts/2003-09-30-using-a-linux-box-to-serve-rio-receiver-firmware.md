---
title: "Using a Linux box to serve Rio Receiver Firmware"
date: 2003-09-30T23:00:00.000Z
x-drupal-nid: 108
x-needs-review: 2003-09-30T23:00:00.000Z
---
First, you'll need to install a stock NFS server. On Debian, that's as simple as "apt-get install nfs-user-server".

You'll then need to export the relevant directory to the Receiver. When connecting, it attempts to mount "/tftpboot/ip-address", where _ip-address_ is the dotted quad form of the IP address assigned by the DHCP server. The default software responds to all mount requests with the same data, but with a Linux box, you'll need to add an export explicitly.

Just add the following line to /etc/exports:

<div class="snippet">
    /tftpboot/192.168.0.30 192.168.0.30(ro,no_root_squash,insecure)

</div>

Obviously, replace the 192.168.0.30 bit with the IP address of your Rio Receiver. Because of the way that the Receiver specifies its IP address in the mount point, you might want to configure your DHCP server to always assign the same address to each Receiver. On Debian, that's as simple as:
<div class="snippet">
      host aurora {
        hardware ethernet 00:90:00:11:5b:7a;
        fixed-address 192.168.0.30;
      }

</div>

I've got my Receiver configured with a hostname, which requires an extra step to set up the DNS database.
## Extracting the Firmware

The `receiver.arf` file included with the Receiver software is just a TAR-format archive. Simply copy it to your Linux box, and extract it into the /tftpboot directory:

<div class="snippet">
    # mkdir -p /tftpboot/receiver
    # cd /tftpboot/receiver
    # tar xf /path/to/receiver.arf
    # ln -s /tftpboot/receiver /tftpboot/192.168.0.30

</div>

And, now, when you turn on the Receiver, it should successfully download its firmware from your Linux box.

* * *

## Other Links

*   The [JReceiver pages](http://jreceiver.sourceforge.net/) on SourceForge.
*   Jeff Mock's page about [hacking the Receiver](http://www.mock.com/receiver/).