---
title: "NBehave Notes"
date: 2010-05-20T08:55:37.000Z
x-drupal-nid: 248
x-needs-review: 2010-05-20T08:55:37.000Z
---
We’ve just started using NBehave, so this is a random selection of notes:

If you try to use NBehave-Console with a .NET 4.0-compiled assembly, you’ll see a BadImageFormatException. You can fix this by creating the NBehave-Console.exe.config file, as follows:

<pre class="code"><span style="color: blue"><?</span><span style="color: #a31515">xml </span><span style="color: red">version</span><span style="color: blue">=</span>"<span style="color: blue">1.0</span>" <span style="color: red">encoding</span><span style="color: blue">=</span>"<span style="color: blue">utf-8</span>" <span style="color: blue">?>
<</span><span style="color: #a31515">configuration</span><span style="color: blue">>
  <</span><span style="color: #a31515">startup</span><span style="color: blue">>
    <</span><span style="color: #a31515">supportedRuntime </span><span style="color: red">version</span><span style="color: blue">=</span>"<span style="color: blue">v4.0.30319</span>"<span style="color: blue">/>
  </</span><span style="color: #a31515">startup</span><span style="color: blue">>
</</span><span style="color: #a31515">configuration</span><span style="color: blue">>
</span></pre>

This makes it use the v4.0 CLR. See [this question on StackOverflow](http://stackoverflow.com/questions/2046089/force-an-application-to-run-under-specific-net-runtime-version).

NBehave’s command-line parsing is a bit flakey. If you pass /scenarioFile (rather than /scenarioFiles), the invalid switch is **ignored**; no error is raised.

Similarly, it ignores unrecognised lines in the text scenario files, so make sure that you type “When”, “Given”, etc., correctly.

Also, NBehave-Console.exe is compiled for “Any CPU”, so if you’re testing stuff that requires 32-bit DLLs (e.g. COM components), you’ll need to use CORFLAGS /32BIT+ to fix up the EXE.