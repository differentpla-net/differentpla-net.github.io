---
title: "CPropertySheet vs CPropertySheetEx in Visual Studio .NET 2003"
date: 2004-01-08T17:05:00.000Z
tags: mfc visual-studio
---
The program that I'm working on at the moment needs a wizard to walk the user through something. I copied over some
files from a [Wizard97 demo project](/files/wizard97.zip) that I wrote a while ago. It all seemed to be going well.

Except, that is, that IntelliSense in Visual Studio kept pretending that my classes were derived from `CPropertySheet`
or `CPropertyPage`, rather than `CPropertySheetEx` or `CPropertyPageEx` respectively.

This was confusing.

It turns out that at some point between VC6 and VS.NET, Microsoft changed the definition of `CPropertySheetEx` and
`CPropertyPageEx` in the MFC headers. They are the same -- one is a #define of the other.

So, that's IntelliSense 1, Roger 0.
