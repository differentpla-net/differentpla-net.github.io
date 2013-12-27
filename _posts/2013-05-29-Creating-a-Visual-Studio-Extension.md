---
layout: post
title: "Creating a Visual Studio Extension"
date: 2013-05-29T17:21:36.367Z
tags: visual-studio-2012 vsix vs-welcome-page
alias: /post/UaY5jQFqM2ZBAAAC/creating-a-visual-studio-extension
---

The next major piece is the Visual Studio extension, so let's get on with that. 
Assuming that you've installed the Visual Studio 2012 SDK, it's as simple as 
creating a new "Visual Studio Package" (it's under "Visual C#" / "Extensibility"):

1. Choose 'C#' as the language.
2. Generate a new SNK file, rather than use an existing one.
3. Fill in the basic package information (name, etc.). Use the default icon.
4. We'll need a menu command. Name it "View Welcome Page" and give it the ID "cmdidViewWelcomePage".
5. Don't bother with unit or integration tests. We've only got a weekend, this is a spike, and there's going to be almost no code in here anyway.

Fire it up; you'll get a new (experimental) instance of Visual Studio, and our 
extension adds a menu item to the top of the Tools menu.

### Moving the menu item ###

We want our menu item to be in the View menu, so we'll have to move it. Open the
`StartPage.vsct` file and change `IDM_VS_MENU_TOOLS` to `IDM_VS_MENU_VIEW`. 
Build the project and check that it's moved.

### Hooking solution open events ###

To hook the solution open events, we need to subscribe to the `ISolutionEvents` 
interface in Visual Studio. Before we can do that, however, we need to tell Visual 
Studio to load our extension early enough.

To do this, we need to add the `ProvideAutoLoad` attribute to our package class:

    // We need to load early enough to be able to hook solution events.
    // VSConstants.UICONTEXT_NoSolution
    [ProvideAutoLoad("ADFC4E64-0397-11D1-9F4E-00A0C911004F")]

Then we need to hook the solution events. We can do this inside `Initialize`. Refer
to the source code on github for complete details, but it's basically the following:

    _solution = GetService(typeof (SVsSolution)) as IVsSolution;
    if (_solution != null)
        _solution.AdviseSolutionEvents(this, out _dwCookie);

Then we make our package implement `ISolutionEvents`, returning `VSConstants.S_OK`
from everything except `OnAfterOpenSolution`, which is where we want to add our
functionality.
