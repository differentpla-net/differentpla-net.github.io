---
title: "Vaio: ACPI patches"
date: 2003-01-13T16:03:00.000Z
x-drupal-nid: 135
x-needs-review: 2003-01-13T16:03:00.000Z
---
You'll need to install the ACPI patches, and the ACPI daemon. Grab the patches from [here](http://sourceforge.net/project/showfiles.php?group_id=36832), and apply them to the kernel:

<div class="snippet">
    ~/src/linux$ gzip -cd ../acpi-20021212-2.4.20.diff.gz | patch -p1

</div>

The ACPI daemon, `acpid` is included in Debian: `apt-get install acpid`. It comes with a sample script that shuts down the Vaio when you hit the power switch. More information is available [here](http://acpid.sourceforge.net/).

Make sure that the modules automatically load at bootup by adding the following to `/etc/modules`:

<div class="snippet">
    # Load the ACPI modules:
    ac
    battery
    button
    fan
    processor
    thermal

</div>

When you've rebooted your new kernel, you should be able to cleanly shutdown the Vaio by using the power switch.