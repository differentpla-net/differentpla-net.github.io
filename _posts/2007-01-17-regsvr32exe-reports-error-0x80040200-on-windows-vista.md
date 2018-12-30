---
title: "regsvr32.exe reports error 0x80040200 on Windows Vista"
date: 2007-01-17T14:52:59.000Z
redirect_from: /content/2007/01/regsvr32-exe-reports-error-0x80040200-on-windows-vista
---

`0x80040200` is `SELFREG_E_TYPELIB`. This result is returned from MFC ActiveX (.OCX) controls when `AfxOleRegisterTypeLib` fails.

On Windows Vista, this is probably because you have UAC enabled, and RegSvr32.exe doesn't correctly prompt for elevation.

**To work around this**, run regsvr32.exe from an elevated command prompt. To get an elevated command prompt, right-click on the "Command Prompt" shortcut and select "Run as Administrator".

Alternatively, you can turn off UAC. If you're going to do this, I recommend having separate administrator and non-administrator accounts.
