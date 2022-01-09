---
title: "Tweaking TeamCity"
date: 2008-01-12T14:11:52.000Z
---
I just started using TeamCity Professional Edition for some personal projects (and we're evaluating the Enterprise edition for use at work).

One of the things I've done is generate a version number in my NAnt script, and [put it in teamcity-info.xml](http://www.jetbrains.net/confluence/display/TCD3/Including+Third-Party+Reports+in+the+Build+Results#IncludingThird-PartyReportsintheBuildResults-TCInfoXML). This results in a nice version number being displayed in TeamCity.

Unfortunately, the column in the Overview page is a little narrow, so the longer version numbers don't display correctly.

To fix it, open webapps\ROOT\css\overview.css and change the width of td.buildNumber from 6em to 9em.

Unfortunately, it looks like getting rid of the # prefix is going to take a bit more hacking on .JSP files. I don't know if I can be bothered.
