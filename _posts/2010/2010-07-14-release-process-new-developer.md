---
title: "Release Process: New Developer"
date: 2010-07-14T13:52:24.000Z
---
This is related to “Can you make a build in one step?” on The Joel Test. But it’s not the same.

Joel is asking whether a complete build can be made in a single step. By pressing one button, do I get an installer containing my latest release?

_This_ is about ensuring that a new developer (or an existing developer at a new machine) can be productive as soon as possible. If it takes even an experienced developer all afternoon and a bunch of manual steps before they can start working with the code, you’ve got a problem.

This is _not_ about getting an installer at the touch of a button. For that, see below. InstallShield is an unneeded expense if only one of your developers knows how to use it.

Some questions:

## Does the machine have everything required already installed on it?

Your answer should be:

> Yes. We use a standard configuration for our developer machines. They have all of the required products and tools (IDE, compiler, SDKs, source control client) installed and configured correctly.

If the project needs something that’s not installed by default on a developer machine, then one of the following should be true:

1.  It should be listed as a prerequisite for that project. A known good version should be available, so that the developer doesn’t download and install a different version from the internet. This probably means that you need a wiki for your team, explaining the build instructions. It must be kept up to date if someone adds a new prerequisite. The wiki is also a useful place to keep “bootstrap” scripts. While most of your scripts should be kept in source control, you might have a script that pulls those out of source control, and by definition, can’t be _in_ source control.
2.  It should be included in the project repository/depot. For example, you might have a “3rdParty” folder at the root of your depot, or you might have an “Ext” folder next to your solution file.

## How much work is involved in getting the latest copy of the source code?

For example, if you've got your source code in Perforce, you need to configure a workspace view, which controls how files in the depot are mapped to locations in your local workspace. Is this documented somewhere? Or, ideally: do you have an up-to-date script that sets this up for a new developer?

If you're using Subversion, make sure that any svn:externals are configured correctly.

The _last_ thing that you want to be doing is following a multi-point checklist of foreign git repositories to clone, or Subversion repositories to check out. Moreover, do you really want to depend on an external repository that may not be there tomorrow? Create a "vendor branch" in your home repository and put the external stuff in there.

## How much work is involved in getting that to build?

If you have to run 5 different batch files (or shell scripts, or makefiles, or msbuild projects), you're doing something wrong. You should be able to build enough to be getting on with in a single command. Ideally, loading the project file into your favourite IDE and pressing the button should just work.
