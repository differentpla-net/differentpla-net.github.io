---
title: "Building Shared Libraries (DLLs)"
date: 2001-07-04T07:00:00.000Z
tags: jam
---
The default Jambase file doesn't come with a rule to make DLLs (or shared libraries in the Unix parlance).

The fix consists of cloning the `Main` and `MainFromObjects` rules, and modifying them as follows, to create a `SharedLibrary` rule.

```
rule SharedLibrary
{
	SharedLibraryFromObjects $(<) : $(>:S=$(SUFOBJ)) ;
	Objects $(>) ;
}
```

This is exactly the same as the `Main` rule, except that we've changed the names.

```
rule SharedLibraryFromObjects
{
	local _s _t ;

	# Add grist to file names
	# Add suffix to dll/so

	_s = [ FGristFiles $(>) ] ;
	_t = [ FAppendSuffix $(<) : $(SUFSHR) ] ;

	if $(_t) != $(<)
	{
	    DEPENDS $(<) : $(_t) ;
	    NOTFILE $(<) ;
	}

	# make compiled sources a dependency of target

	DEPENDS exe : $(_t) ;
	DEPENDS $(_t) : $(_s) ;
	MakeLocate $(_t) : $(LOCATE_TARGET) ;

	# Tell jam where it can find the import library
	MakeLocate $(_t:S=$(SUFLIB)) : $(LOCATE_TARGET) ;

	Clean clean : $(_t) ;

	LINKFLAGS on $(_t) += /dll ;
	Link $(_t) : $(_s) ;
}
```

This is a clone of the `MainFromObjects`, except that we've:

1.  Changed the name.
2.  Changed the suffix for DLL/.so files.
3.  Told jam where the import library is going to go.
4.  Set some LINKFLAGS on the target. These are needed in order to let Microsoft LINK know that it's building a DLL.
