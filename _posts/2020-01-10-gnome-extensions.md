---
title: "Gnome Extensions"
date: 2020-01-10T20:07:00.000Z
tags: ubuntu ubuntu-install
layout: series
series: ubuntu-install
---

I installed the following extensions:

## Windows-style taskbar

- Hide top bar. This extension didn't seem to do anything.
- Dash to panel. This replaces the top bar with a Windows-style task bar. I haven't figured out to stop it from hiding under maximised windows, though. This is annoying.
- Dash to dock. This allows me to have a taskbar on my second monitor. I configured it to "Show the dock on: Secondary monitor 1", at the bottom. Turn off "Show favourite applications". Turn on "Isolate monitors". Turn off "Show Applications icon".

"Dash to panel" and "Dash to dock" aren't quite the same size, but I'll live with it.

## Update, about 20 minutes later

The newest version (26, on https://extensions.gnome.org/extension/1160/dash-to-panel/) of Dash to panel is much improved. I've disabled "Dash to dock" and I'm experimenting with it. Note that you need to restart Gnome Shell (Alt+F2, r) in order to turn on the "Display panels on all monitors" setting.

I installed it by following these instructions: https://linuxconfig.org/how-to-install-gnome-shell-extensions-from-zip-file-using-command-line-on-ubuntu-18-04-bionic-beaver-linux, but with `gnome-shell-extension-tool -r` at the end.

## Others

- Alternatetab. Because, seriously, why would you want two different keystrokes to switch between top-level windows? I use something similar on macOS.
- Audio output switcher. I've got three different audio outputs on my PC; this is essential.
- Media player indicator. Not entirely sure this is needed, since the controls appear when clicking on the clock anyway.
