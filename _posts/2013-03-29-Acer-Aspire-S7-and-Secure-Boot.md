---
layout: post
title: "Acer Aspire S7 and Secure Boot"
date: 2013-03-29T13:59:28.332Z
tags: acer-aspire-s7 secure-boot
alias: /post/UVWeVLN0lg0cAAAB/acer-aspire-s7-and-secure-boot
---

I just bought myself a shiny new Acer Aspire S7-391, with a view towards nuking Windows 8 and to install Linux Mint on it. Stupidly, I didn't consider that Windows 8 logo certification now requires a UEFI BIOS and Secure Boot.

Fortunately, on the S7, Secure Boot is disabled by default.

You can verify this by pressing F2 (Fn+2) when the laptop boots up. When you get into the BIOS setup screen, and go to the "Boot" page, you'll see that "Secure Boot" is set to "Disabled".

*Note that the S7 puts the function keys F1-F12 on the same keys as the numbers. You have to press Fn+1 for F1, Fn+2 for F2, etc.*

If, for some reason, you want to enable Secure Boot, you need to first set a supervisor password: on the "Security" page, select "Set Supervisor Password" and press Enter. The password can be up to 8 characters (A-Za-z0-9). You'll be prompted for this password every time you enter the BIOS configuration screen in future. Make sure you remember what it is, since you get 3 attempts before bricking the laptop.

Once you've set a supervisor password, you can go to the "Boot" page and set "Secure Boot" to "Enabled". After you do this, the "Security" page will allow you to configure the Secure Boot parameters.

That said, I've not attempted to install Linux Mint just yet -- I thought I'd try Windows 8 for a few days first.

