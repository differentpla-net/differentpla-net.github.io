---
title: "Jam - Separate Release/Debug Target Directories"
date: 2002-02-01T12:05:00.000Z
tags: jam
---
By default, AppWizard-generated applications use separate directories for the output of the different debug and release builds. We'd like to replicate that functionality.

This is controlled by the `LOCATE_TARGET` variable. It's initialised by the `SubDir` rule to be the same as the `SEARCH_SOURCE` variable, unless `ALL_LOCATE_TARGET` is set.

However, if we set `ALL_LOCATE_TARGET` to, e.g. `Debug` in our `Jamrules` file, or using the `-s` switch to jam, the object files and targets are all built in exactly the same directory. It's not relative to the subdirectory. This could cause confusion if directories generate object files with the same names.

The comments in `Jambase` state that the `LOCATE_TARGET` variable should be set after invoking the `SubDir` rule, if required. This is a pain. We much prefer the following change to the `SubDir` rule:

```
       # directory should not hold object files, LOCATE_TARGET can
       # subsequently be redefined.

       **local path = [ FDirName $(SUBDIR) $(TARGET_PREFIX) ] ;

       SEARCH_SOURCE = $(SUBDIR) ;
       LOCATE_SOURCE = $(ALL_LOCATE_TARGET) $(path) ;
       LOCATE_TARGET = $(ALL_LOCATE_TARGET) $(path) ;
       SOURCE_GRIST = $(path) ;**

       # Reset per-directory ccflags, hdrs
```

This allows us to place a rule like the following in our `Jamrules` file:

```
if ! $(DEBUG) {
	ECHO Assuming DEBUG=1 ;
	DEBUG = 1 ;
}

if $(DEBUG) = 0 {
	TARGET_PREFIX = Release ;
}
else {
	TARGET_PREFIX = Debug ;
}
```
