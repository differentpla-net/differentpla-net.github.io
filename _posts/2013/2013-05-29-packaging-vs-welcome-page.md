---
title: Packaging vs-welcome-page
date: 2013-05-29T17:25:57Z
layout: series
series: visual-studio-extension-in-a-weekend
---
Next problem: packaging the `vs-welcome-page` extension.

How to get the web application installed with the VS package? There doesn't appear to be an easy way to add arbitrary
files to a VSIX package. You can do it by adding references to dependencies, but that doesn't help with the content
(CSS, HTML files, etc).

So: what to do? I could use an MSI-based installation, but:

* It seems like overkill for an extension this simple.
* If we need an elevated installation, why not simply use the opportunity to register the URL ACL? If you remember, this
  is why I decided to use IIS Express in the first place.

I'd like this extension to be available on the VS gallery. Does this have any implications?

## Spike: Looking at some VSIX packages

I had a quick look at what's on the VS gallery, and almost everything appears to be a VSIX file.

Some questions on Stack Overflow suggest that it's not really possible. Therefore: a couple of options occur:

1. Embed the whole thing in the package extension and extract it (in the step where we would have been copying it).
2. Embed the resources and use a reference to include the DLL. This might need some finesse w.r.t. Web.config.

OK. How do we pull the files into the VSIX? Let's try it with a simple reference. That sorta-works: all of the DLLs and
content end up in the installation folder. Where that goes wrong is that the DLLs don't end up in a `bin` folder, and
ASP.NET can't find them. Adding a probing/privatePath=. doesn't seem to work.

OK. Let's include the whole thing. Options:

1. Put the whole lot in a ZIP file and embed it as a resource.
2. See if we can merely include the ZIP file as content and have it turn up in the VSIX file.

OK. Can we do the second? Doesn't look like it. Attempted to add a file as Copy to output directory -- it didn't turn up
in the output.

## Assets

After some more investigation, it appears that you can include custom assets in the VSIX.

To do this, go to the Assets tab in the manifest editor and add a new one. You can use a custom type, specify a file,
and it gets included in the VSIX.

This isn't massively scalable -- how do we include an entire project output?

If you include a project in the current solution, the whole lot turns up. This is basically equivalent to the reference,
and because it ignores the directory, the whole lot turns up in the working directory, which was a problem before.

More poking around: if you add an asset from outside the current project, it gets copied into the current folder. This
implies that we'll have difficulty with keeping things up to date.

So: it looks like we need to:

1. Get the web application zipped up.
2. Copy that (as a build step) to the project directory.
3. Include that as an asset.

So: how to ZIP everything up?

    msbuild .\StartPage.WebApplication.csproj /p:DeployOnBuild=true /p:PublishProfile='Publish'

Then ZIP it (or merely include it in the other project)

OK, we know how to publish it from the command line -- how to get that into the VS build process?

Getting it to build is relatively easy -- getting the other project to build afterwards is the hard part.

OK. That's done -- dependencies and zipping.
