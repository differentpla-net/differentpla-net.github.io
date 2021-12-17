---
title: A Visual Studio Extension in a Weekend
date: 2013-05-29T15:05:42Z
---
I wanted to write a Visual Studio extension (for VS2012) that, when you open a
solution, renders and displays Markdown documentation in Visual Studio.

This enables you to include welcome documentation for developers who've just
joined the project

My aim was to write this in a single weekend. I made a load of notes as I went
along, and I've just finished writing them up as a series of blog posts, which
you will find here.

Architecture
--

A **VSIX-deployed** Visual Studio extension, written in C#. When you open a
solution, it will start **IIS Express**, running a small web application
(using **Nancy**) to render the markdown into HTML. It will open the built-in
web browser page in Visual Studio, pointing at this application.

Why IIS Express? Initially, simplicity.

I considered adding a document window in Visual Studio, hosting a WebBrowser
control, because this would give me more control over what was rendered, and 
would allow bi-directional communication between the rendered view and the
extension. I decided not to do this, because I only had a weekend, and I don't
have the first idea how to host a custom document window inside Visual Studio.

The blog posts
--

* [A Visual Studio Extension in a Weekend: Introduction](/b/2013/41/29/a-visual-studio-extension-in-a-weekend)
* [Running IIS Express](/b/2013/41/29/running-iis-express)
* [Spike: Providing parameters to IIS Express-hosted applications](/b/2013/41/29/spike:-providing-parameters-to-iis-express-hosted-applications)
* [Displaying a web page in Visual Studio](/b/2013/41/29/displaying-a-web-page-in-visual-studio)
* [Rendering Markdown in .NET](/b/2013/41/29/rendering-markdown-in-net)
* [Creating a Visual Studio Extension](/b/2013/41/29/creating-a-visual-studio-extension)
* [Killing IIS Express](/b/2013/41/29/killing-iis-express)
* [Packaging vs-welcome-page](/b/2013/41/29/packaging-vs-welcome-page)
* A week later: [Using IIS Express in vs-welcome-page](/b/2013/41/29/using-iis-express-in-vs-welcome-page) -- revisiting this decision.
