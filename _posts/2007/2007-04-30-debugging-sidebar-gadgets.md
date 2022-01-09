---
title: "Debugging Sidebar Gadgets"
date: 2007-04-30T16:01:29.000Z
x-drupal-nid: 178
x-needs-review: 2007-04-30T16:01:29.000Z
redirect_from: /content/2007/04/debugging-sidebar-gadgets
---
Apparently, you can debug JScript in Visual Studio.

The documentation for this is horrendous, though. Almost every resource talks about debugging ASP.NET programs. If you're writing a Windows Vista Sidebar Gadget, this isn't a lot of use.

First, you need to enable script debugging:

1.  In Internet Explorer, go to Tools -> Options.
2.  Click on the Advanced tab.
3.  Ensure that "Disable script debugging (Other)" is unchecked.

Then, to debug your gadget:

1.  Ensure that your gadget is installed on the sidebar.
2.  In Visual Studio, go to Debug -> Attach to Process.
3.  In the **Available Processes** list, you'll see one or more instances of `sidebar.exe`. There's one for each loaded gadget, with the Title column telling you which one is which. The title is taken from your Gadget XML.
4.  Click on the one you want to debug.
5.  Note that the **Attach to** label will show you which debugger will be invoked (normally it's the Script one).
6.  Click the **Attach** button.
    [img_assist|nid=179|title=|desc=|link=none|align=left|width=640|height=444]

    Once you've done this, you should be able to place breakpoints in your Javascript and then step through your code.

    If you want to debug your gadget's startup code, you'll need to do something like this:

    <pre>function onLoad()
    {
        debugBreak;

        // Rest of code goes here
    }</pre>

    When the scripting engine reaches the `debugBreak;` line, you'll get a message box saying "A Runtime Error has occurred. Do you wish to Debug?". If you click on the Yes button, you can debug using Visual Studio. Note that you'll have to use the "Set Next Statement" command to skip over the offending statement.
