Build Process
==

As a developer, when I'm investigating a support case, and particularly where it's a post-mortem debugging job (todo: link to tag), I need to know exactly which build of the software that the customer is talking about.

I need this information so that:

* I can install the exact same binaries on a test machine so that I can attempt to reproduce the problem.
* I can find the exact source files used to build the binary, so that I can figure out exactly where the bug is.
* For post-mortem debugging, I can find the debugging symbol files. When debugging with WinDbg, it's much easier if the crash dump file, the binaries and the debug symbols all match.

The way that we usually handle this is by ensuring that each release of the software has a unique version number. That is: for a given version number (e.g. 1.0.2.98) reported by the customer, there should be a unique set of binaries, debug symbols and source files.

Source Code
--

One way to find the source files again is to ensure that they're tagged in your revision control system. In this way, each build (e.g. 1.0.2.98) exists as a single tag (e.g., in Subversion, you might use `/tags/1.0.2.98`). You can often configure your build server to do this automatically after each build.

Alternatively, you can associate a changelist number with the particular build. It's a good idea to add this to the (TODO: which bit of the VERSIONINFO is this).

Of course, this assumes that you have a simple numeric changelist number. TODO: Examples of where you might not have this, and what are you going to do about it?

----

TODO: How does WinDbg get from a dump file to the binaries?

TODO: How does it get from there to the PDB files?

TODO: Storing source file references in the PDB files.




As a developer, when I'm investigating a support case, one thing that I want to know is exactly which build of the software that the customer is talking about.

Where possible, then, I want a clear association between the binaries that the customer is talking about, and the source files in our revision control system. For post-mortem debugging, it's also easier if I can dig out the exact binaries that were shipped to the user, and also the corresponding symbol files.

Usually, this relationship is most easily expressed by the version number. That is: the customer has a version number such as "1.0.2.98", the version number to be easily visible to the user (for phone support cases), and it must be present in the log files, so that I can look it up in source control and artefacts.

VERSIONINFO, changelists, source control tags, etc.


TODO: the fact that we build the binaries multiple times, potentially from the same source code. Ignore the fact that they could be built on different build agents, because that leads us into a discussion of whether to store build tools in source control.

However: that leads us into an interesting discussion of whether it's worth creating a VM snapshot for each release that goes out of the door. More particularly, because we don't know which release is going out the door until much (?) later: is it possible to snapshot a build machine after each build, in such a way that it can be done quickly, and in such a way that it's usable from that snapshot? Moreover: how do I collapse snapshots so that I'm only keeping around the ones corresponding to actual releases?

All that said: are daily snapshots, or snapshots after software installed, sufficient?


