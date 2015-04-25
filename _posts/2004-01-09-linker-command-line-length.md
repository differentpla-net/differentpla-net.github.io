---
title: "Linker Command Line Length"
date: 2004-01-09T10:37:00.000Z
x-drupal-nid: 116
x-needs-review: 2004-01-09T10:37:00.000Z
---
Jam imposes a hard limit of 996 characters on command lines when built on NT. This limit is higher for other operating systems, and can actually be raised to around 10Kb on Windows 2000\. However, it's still not high enough for some link actions.

We'd like, therefore, to place the linker actions in a response file, and invoke the linker with that instead. Replace the "actions Link..." clause in the NT-specific section of `Jambase` with this:

<pre>	rule Link
	{
		MODE on $(<) = $(EXEMODE) ;
		LINKFLAGS on $(<) += $(LINKFLAGS) $(SUBDIRLINKFLAGS) ;
		Chmod $(<) ;

		local _i ;
		StartLink $(<) : $(>) ;
		for _i in $(>)
		{
			LinkItems $(<) : $(_i) ;
		}
		FinishLink $(<) : $(>) ;
	}

	rule StartLink
	{
		Clean clean : $(<:S=.rsp) ;
	}

	actions quietly Link
	{
	}

	# We have to touch the file first,
	# or the delete will fail, stopping the build.
	actions quietly StartLink
	{
		$(TOUCH) $(<:S=.rsp)
		$(RM) $(<:S=.rsp)
	}

	actions together piecemeal quietly LinkItems
	{
		ECHO $(>) >> $(<:S=.rsp)
	}

	actions FinishLink bind NEEDLIBS
	{
		$(LINK) $(LINKFLAGS) /out:$(<) $(UNDEFS) @$(<:S=.rsp) $(NEEDLIBS) $(LINKLIBS)
	}</pre>

Remember to set `TOUCH` to something sensible.