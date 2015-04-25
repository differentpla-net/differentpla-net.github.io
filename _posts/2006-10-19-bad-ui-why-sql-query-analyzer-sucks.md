---
title: "Bad UI: Why SQL Query Analyzer sucks"
date: 2006-10-19T10:40:31.000Z
x-drupal-nid: 69
x-needs-review: 2006-10-19T10:40:31.000Z
---
I'm doing a bunch of SQL hacking at the moment, and I've decided to use SQL Query Analyzer as a good template for how not to write a text editor.

Note that this only applies to SQL Query Analyzer in SQL 2000\. SQL Management Studio in SQL 2005 has its own set of suckage. I'm sure I'll rant about that when I've used it more.

1.  It doesn't shorten filenames displayed in the title bar. I have absolutely no idea which file I'm looking at, because they're all in the same directory.  
     Note: simply sticking an ellipsis somewhere in the name isn't really good enough either. If I'm looking at the same file in two different directories (for example two different branches in source control), you need to show that, too. I want to see something like "...branches\wherever\...\filename.sql".
2.  The Ctrl+Tab behaviour is completely hosed. It doesn't work anything like other MDI Windows applications. While I'm holding down Ctrl, it should rotate through the windows. Pressing and releasing Ctrl+Tab twice should toggle between the top two windows in the stack. In Query Analyzer, it doesn't.
3.  Ctrl+Left, Ctrl+Right are broken.
4.  Home should take me to the beginning of the text on the line, and then alternate between there and column 1.
5.  The cursor should have a virtual column position, allowing it to float at the right-hand side.

</rant>