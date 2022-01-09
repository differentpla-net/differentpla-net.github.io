---
title: "Source Server indexing fails with \"SVN: Can't Get details for <sourcefile>\""
date: 2009-04-23T09:34:24.000Z
x-drupal-nid: 234
x-needs-review: 2009-04-23T09:34:24.000Z
redirect_from: /content/2009/04/source-server-indexing-fails-svn-cant-get-details-sourcefile
---
I'm making some improvements to our build scripts at work, and I ran into a problem where Source Server indexing wasn't working.

We use Subversion, so our NAnt scripts were running the following:

```xml
<exec program="${path::combine(debugging.tools, 'srcsrv\ssindex.cmd')}">
  <arg value="/System=${this.system}"/>
  <arg value="/Source=${this.source}"/>
  <arg value="/Symbols=${this.symbols}"/>
  <arg value="/Debug" if="${this.debug == 'true'}"/>
</exec>
```

Obviously, I was making my changes locally before pushing them to the build server, but I kept running into the following error:

```
ssindex.cmd [STATUS] : Processing svn.exe properties output ...
...ndex.cmd [WARN  ] SVN: Can't Get details for d:\source\foo\trunk\src\bar
...ndex.cmd [WARN  ] SVN: Can't Get details for d:\source\foo\trunk\src\bar\baz.cpp
```

(Obviously, the names have been changed to protect the innocent)

After a lot of head scratching, I discovered the problem: The Source Server indexing scripts use Perl. Our build agents
are running ActiveState Perl, and my box is running Cygwin Perl. They're both running Win32 builds of SVN.

The Win32 build of SVN uses DOS-style line endings (CR/LF), I had the Cygwin build of Perl configured to expect
Unix-style line endings (plain LF). This causes some regular expressions that use '$' to fail to match a line, because
there's still a stray carriage return (\r) character hanging around, even after you've used chomp.

One of my colleagues found a couple of links about [this problem](http://www.nabble.com/Concatenation-working-weirdly-on-Unix-td9174043.html)
and how to [open](http://perldoc.perl.org/functions/open.html) files in Perl, with explicit CR/LF support, but I'm just going to
ensure that ActiveState Perl is in `%PATH%` first, at least for now.
