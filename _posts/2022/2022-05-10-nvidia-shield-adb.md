---
title: "Enable Network Debugging on nVidia Shield"
date: 2022-05-10T10:43:00Z
tags: android
---

Here's how to debug your apps over the network on an nVidia Shield device.

On the Shield:

1. Navigate to `Settings` / `Device Preferences` / `About`.
2. Scroll down to Build and press select 7 times. This unhides the developer options.
3. Navigate to `Settings` / `Device Preferences` / `Developer options` (it's right at the bottom).
4. `Enable developer options`.
5. Under `Debugging`, enable `Network Debugging`. Make a note of the IP address, e.g. `192.168.1.221`.

On your PC:

1. Run `adb connect 192.168.1.221` (replace with the IP address above).
2. On the Shield, approve the debug connection.

I'm using Flutter in Visual Studio Code; this resulted in `SHIELD Android TV (android-arm64)` appearing in the device
list. Selecting this device and running the app should result in it running on your nVidia Shield.
