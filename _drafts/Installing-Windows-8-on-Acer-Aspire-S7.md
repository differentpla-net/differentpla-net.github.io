Installing Windows 8 Pro on Acer Aspire S7
==

**Note:** Using the "Windows 8 (multiple editions) (x64) - DVD (English)" download from MSDN.

1. Make sure that the ISO file you've downloaded is not corrupt -- this caused me a bunch of grief.
2. Format a bootable USB stick as FAT32.
3. Mount the ISO.
4. Copy the files from the ISO to the USB stick.
5. Add the Intel RAID drivers to the USB stick.
6. Delete & Recreate RAID array.
7. In the Acer's BIOS, select UEFI; enable F12 boot menu.
8. Restart the Acer.
9. Press `Fn-=` (F12).
10. Select the USB stick.
11. Add the Intel drivers.
12. Continue with the installation.
13. Activate Windows.

Note that the 128GB SDD is two 64GB SSD drives configured as RAID0. You'll need the Intel RAID drivers.

Once you're in Windows 8, you'll need to install the wireless driver. This is available on the Acer support site.

From there, Windows can mostly figure it out.

In depth
--------

### Comparing Checksums

The MSDN and TechNet subscribers page should show the SHA1 hash of the ISO file. To verify the file is downloaded correctly, use the following PowerShell command:

    Get-Hash -Algorithm SHA1 en_windows_8_x64_dvd_etc.iso

Non-MSDN versions are just called Windows-8-Pro-Upgrade-Retail.iso; since these appear to be stamped with the product key when you download them, the checksum varies.

### Formatting a bootable USB stick as FAT32

**Important:** If you use the *Windows 7 USB/DVD Download Tool*, it will format the USB stick as NTFS, which isn't readable by the UEFI BIOS in the Acer Aspire S7.

You'll need to use `DISKPART` to format the USB stick. Run `DISKPART` and then do the following:

	list disk

...look for the USB stick (hint: it'll be the one with a size of about 4000MB or 8000MB). In my case, it was "Disk 6".

	select disk 6
	clean
	create partition primary
	select partition 1
	format fs=fat32 quick
	active
	assign
        exit

This sequence of commands:

	1. Wipes the disk.
	2. Creates a new partition and selects it.
	3. Formats it as FAT32.
	4. Marks it as active (bootable).
	5. Assigns a drive letter.
        6. Exits from diskpart.

### Mount the ISO

For mounting ISO files in Windows 7, I use MagicDisc.

### Copy the files

Use `XCOPY`:

	XCOPY /E Y:\*.* E:\

### Add the Intel RAID Drivers

From the Acer support site, download the Intel SATA AHCI Driver (currently 11.5.0.1207) for the Acer Aspire S7-391.

Then copy the files to the same USB stick that you'll be using to install Windows 8.

### Also remember this bit (wiping RAID array)

[http://acer.custhelp.com/app/answers/detail/a_id/28526/~/reinstall-windows-on-the-aspire-s7-191](http://acer.custhelp.com/app/answers/detail/a_id/28526/~/reinstall-windows-on-the-aspire-s7-191)



When you're installing Windows, and it's offering you two hard disks, select "Load driver", and point it at the `AHCI_Intel_11.5.0.1207_Win8x64\F6 Install Floppy Create for 32 and 64 bit Windows\f6flpy-x64` folder.

Note that the disk will appear anyway. If it appears as two volumes, you broke the RAID array. Fix that first.

You should install the Intel RAID drivers anyway, otherwise it won't boot after you finish the installation.

### Enter product key to activate Windows 8 Pro (MSDN)

MSDN copies of Windows 8 don't ask for a key, install Windows 8 (not Pro) which isn't activated when installed.

To activate it as Windows 8 Pro, go to Control Panel / System and Security / System, then click on "Get more features with a new edition of Windows". Click "I already have a product key". Enter the MSDN product key (retail purchased copies won't work), and click Next. 

### Activate retail Windows 8

If you get the message "The product key entered does not match any of the Windows images available for installation. Enter a different product key." while attempting to install it, you need to create a file 'PID.txt' in the 'sources' folder of the USB stick, containing the following:

    [PID]
    Value=xxxxx-xxxxx-xxxxx-xxxxx-xxxxx

You can do this from the Windows 8 / Repair / Advanced / Command Prompt, but remember to save the file to C:\sources\PID.txt (the USB stick), rather than X:\sources\PID.txt (ramdisk).

But! Even once you've done that, Windows will refuse to activate. See http://answers.microsoft.com/en-us/windows/forum/windows_8-windows_install/windows-8-pro-product-key-doesnt-work/a08e8a9f-db56-480a-8ee6-1310f9907bbd for the answer, but:

1. In regedit, go to `HKEY_LOCAL_MACHINE\Software\Microsoft\Windows\CurrentVersion\Setup\OOBE` and change the value `MediaBootInstall` from `1` to `0`.
2. Open an Administrator Command Prompt, and enter `slmgr.vbs /rearm`.
3. Reboot.
4. Run `slui 3`, and enter your product key again.

#### Links

- https://technologychatcorner.com/index.php?topic=11.0
- http://answers.microsoft.com/en-us/windows/forum/windows_8-windows_install/windows-8-pro-product-key-doesnt-work/a08e8a9f-db56-480a-8ee6-1310f9907bbd

