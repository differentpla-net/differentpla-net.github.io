---
layout: post
title: "Displaying a web page in Visual Studio"
date: 2013-05-29T15:35:04.007Z
tags: visual-studio-2012 vs-welcome-page
alias: /post/UaYgS_DfiVJBAAAE/displaying-a-web-page-in-visual-studio
---

Using the web browser that's built into Visual Studio is dead simple:

    var dte = (DTE)GetService(typeof(DTE));
    dte.ItemOperations.Navigate(url);
