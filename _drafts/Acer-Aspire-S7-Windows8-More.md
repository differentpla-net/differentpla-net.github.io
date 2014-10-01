# Acer Aspire S7: Windows 8 Installation; more notes.

Getting into the BIOS: F2 (Fn+2).
F12 Boot Menu: Enabled (on the Main tab).
SATA Mode: RAID Mode.
xHCI Support: Enabled.

When pressing F12, select (e.g.) "USB HDD: SanDisk Cruzer Blade".

Which type of installation do you want?
Select Custom: Install Windows only (advanced).

Where do you want to install Windows?

- this is the point where you need the Intel drivers.
- Click "Load driver".

I had them on the USB stick; browse to (probably) `C:\AHCI_Interl_11.5.0.1207_Win8x64\F6 Install Floppy Create for 32 and 64 bit\f6flpy-x64`. The driver should appear in the list; choose it.

At this point, your hard disk should appear.

Click "Drive options (advanced)"; delete all of the partitions. You did back up first, though, right?

When you've done that, select "Drive 2 Unallocated Space"; 119.2GB.

Then wait.

