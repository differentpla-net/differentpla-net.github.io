---
title: Displaying a web page in Visual Studio
date: 2013-05-29T15:35:04Z
layout: series
series: visual-studio-extension-in-a-weekend
---
Using the web browser that's built into Visual Studio is dead simple:

```c#
var dte = (DTE)GetService(typeof(DTE));
dte.ItemOperations.Navigate(url);
```
