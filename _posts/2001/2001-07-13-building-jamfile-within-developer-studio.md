---
title: "Building an MFC Application with Jam: Building within Developer Studio"
date: 2001-07-13T11:42:08.000Z
tags: jam
---

While getting our MFC application to build with Jam, we got to a point where it would compile and link, but wouldn't
run.  This calls for a trip into the debugger.  The simplest thing might be to just run the executable within Developer
Studio.

However, while we're here, we might as well start looking at how to get our application built with jam when we hit F7 in
Developer Studio:

- Delete the existing .dsp and .dsw files.  You'll need to quit Developer Studio for this step.
- Run Developer Studio.  Create a new "Makefile" project, called "mfc_exe".
- The debug command line is (for now) `jam -f /jam-test/Jambase`.  The output file is correct. The rebuild all switch is
  `-a`.
- Enter the same details for the release settings.  Hit "Finish".
- Press F7. You should see jam running in the output window, building your project.

If you've not put MSVCNT in your global environment variables, you'll need to specify it using e.g.
`-sMSVCNT=P:\VStudio\VC98` on the jam command line.

You might need to make sure that the jam executable is in the path for Visual C++. Change the settings under Tools /
Options / Directories.
