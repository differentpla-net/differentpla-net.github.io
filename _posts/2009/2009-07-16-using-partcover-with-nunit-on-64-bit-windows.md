---
title: "Using PartCover with NUnit on 64-bit Windows"
date: 2009-07-16T13:04:26.000Z
---
Some random notes:

## Running PartCover.exe on x64

If you run it, you'll get the following error message:

<pre>Retrieving the COM class factory for component with CLSID {FB20430E-CDC9-45D7-8453-272268002E08} failed due to the following error: 80040153.</pre>

This is because the COM class requested is 32-bit only, and PartCover is running as a 64-bit app. To fix it, you'll need to use CORFLAGS:
<pre>"%VS90COMNTOOLS%\vsvars32"
CORFLAGS /32BIT+ /FORCE path\to\partcover.exe
</pre>

## Running PartCover with NUnit

First, you need to get nunit-console-x86 working:

<pre>nunit-console-x86 path\to\unit.tests.dll
</pre>

## PartCover inclusions

You'll probably forget this bit (I usually do), but you need to tell PartCover what to include in its coverage analysis:

<pre>partcover --target path\to\nunit-console-x86.exe --target-args path\to\unit.tests.dll --include [*]* --output PartCoverResults.xml
</pre>
