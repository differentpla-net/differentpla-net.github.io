# Acer Spin 11

## Reset Windows 10

This PC is an ex-display model, and I can't log in, so it needs resetting.

1. Turn the PC on.
2. Press Alt+F10; the Acer boot screen displays "Please wait" and then takes
   you to the Windows 10 startup options.
3. At the "Choose an option" screen, select "Troubleshoot".
4. Choose "Reset this PC".
5. Choose "Remove everything".
6. Choose "Just remove my files". You're keeping the PC, so you don't need to
   securely erase everything.

At this point, we'll be taken through the Windows 10 setup process.

## Create recovery media

At some point, I might want to reinstall Windows 10 on this PC, so I'm going to
create some recovery media.

1. Insert a blank USB stick. I used a 32GB stick.
2. Press the Windows button.
3. Type "recovery", and choose the "Create a recovery drive" icon that appears.
4. In the "Recovery Drive" wizard, ensure that "Back up system files to the
   recovery drive." is checked. Press "Next". Wait while the wizard does ...
   whatever it is that it's doing.
5. On the next page, select the USB stick that you inserted.
6. On the next page, choose "Create". **It will erase the USB stick.**
7. Wait. Wait some more. It took over 2 hours.

Note that this recovery media is pre-loaded with Acer's pre-loaded crap, so if
I *do* restore it, I'll probably "refresh" Windows to restore it to pristine.

- https://support.microsoft.com/en-gb/help/4026852/windows-create-a-recovery-drive
- http://uk.pcmag.com/operating-systems-and-platforms/85396/news/how-clean-up-windows-10-with-the-refresh-windows-tool

## BIOS Settings

1. Turn the PC on.
2. Press Alt+F10.
3. At the "Choose an option" screen, select "Troubleshoot".
4. Choose "Advanced Options".
5. Choose "UEFI Firmware Settings". Click "Restart" to confirm. This will take
   you to a traditional-looking BIOS setup screen.
6. On the "Main" tab, enable "Network Boot" and "F12 Boot Menu".
7. Press F10 to "Save and Exit".

## Boot from USB

1. Insert your bootable Linux USB stick.
2. Turn the PC on.
3. While it's starting up, mash the F12 key. This brings up the boot menu.
4. Select "Linux" from the list.
5. Select "Install Ubuntu".

### Disabling HPET

**YMMV** At the point that the installation got to "Configuring hardware", it
slowed to a crawl. Looking at the logs, it kept repeating `hpet1: lost 7161 rtc
interrupts`; so I tried it with `hpet=disable`, as follows:

At step 5, above, instead of pressing Enter to "Install Linux", press "e" to
edit the command line. Using the editor, add `hpet=disable` to the `linux`
line. I added it after `boot=casper`. Then press F10 to boot with that
configuration.

That _did_ get it to stop spamming errors to the log, but it didn't get any
further -- it hung at "installing grub2". See below.

If that doesn't work, I guess my next options are trying a different USB stick
or disabling Wifi, or upgrading the BIOS version.

## Hangs at installing grub2

**YMMV**

Try this:

1. Enter the BIOS (press F2)
2. Go to the "Security" page.
3. Choose "Select an UEFI file as trusted for executing".
4. Navigate to USB0 / efi / boot / grubx64.efi
5. Enter "Linux" as the label.

See https://askubuntu.com/a/980196/158095, and
https://forums.linuxmint.com/viewtopic.php?t=256244, and
https://askubuntu.com/questions/21342/how-can-i-load-ubuntu-when-all-i-have-is-grub

    Installing for x86_64-efi platform.
    general protection fault
    [stack traces]
    grub-install: error: efibootmgr failed to register the boot entry: Unknown error -1.
    Installing for x86_64-efi platform.

## Install Ubuntu

1. Select language; I'll assume "English". Click "Continue".
2. Select Keyboard; I chose "English (UK)".
3. On the next screen, it prompts to connect to a wireless network. I connected
   to my guest WiFi.
4. On the "Updates and other software", I chose "Minimal installation" and
   "Download updates". I did *not* choose "Install third-party software",
   because that required turning off Secure Boot, and I have no idea what I'm
   doing.
5. On the "Installation type" screen, I chose "Erase disk and install Ubuntu".
   I also turned on disk encryption (which apparently requires LVM).
6. For the "Choose a security key" page, I used my password manager to generate
   a suitable key. "Suitable" means: alphanumeric (so that it's easy to type in
   case the keyboard layout changes), and reasonably long (so that it's
   relatively hard to brute-force), but not too long (because it needs typing
   in on every boot).
7. Confirm the disk partitioning scheme.
8. Choose the correct timezone.
9. Create your user account, and name the PC.
10. Wait for the install to finish.

## Security Boot Fail

At one point in the process, I interrupted the installation process and
rebooted the PC. I was presented with a padlock icon and "Security Boot Fail".

Pressing Ctrl+Alt+Del restarted the PC, but pressing Alt+F10 (as above) didn't
work.

To get into the BIOS, press F2.

From this point, you can set a supervisor password. You need to do this in
order to edit the Secure Boot settings.

Then you need to "Select an UEFI file as trusted". **TODO**.

## Links

- Acer, Downloads: https://www.acer.com/ac/en/US/content/support-product/7024?b=1&pn=NX.GMBEK.006
- https://community.acer.com/en/discussion/533717/acer-aspire-es1-111m-linux-mint-installation
