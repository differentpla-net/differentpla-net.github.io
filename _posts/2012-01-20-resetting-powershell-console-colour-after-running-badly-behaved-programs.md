---
title: "Resetting PowerShell console colour after running badly-behaved programs"
date: 2012-01-20T09:30:57.000Z
x-drupal-nid: 276
x-needs-review: 2012-01-20T09:30:57.000Z
---
Some programs write their output in different colours. If they're badly-behaved and don't reset the colours when they've finished, your PowerShell console might be left with the wrong background or foreground colours.

This is particularly problematic, because you can't use <tt>$HOST.UI.RawUI.BackgroundColor</tt> with anything other than the built-in PowerShell colours (which don't match the background used by the default PowerShell shortcut).

The solution is to put <tt>[Console]::ResetColor()</tt> at the top of your custom <tt>prompt</tt> function. You do have one of these in your <tt>$PROFILE</tt>, right?