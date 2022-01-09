---
title: "How do I change the details displayed for my sidebar gadget?"
date: 2007-04-23T09:08:09.000Z
---
When you click on "Show details" in the Windows Vista gadget gallery, Windows displays various information about the selected gadget.

![[img_assist|nid=171|title=|desc=|link=none|align=left|width=640|height=361]](/broken-image-link)

These details come from your Gadget.xml file:

```xml
<?xml version="1.0" encoding="utf-8" ?>
<gadget>
  <name>BasicGadget</name>
  <version>1.0.0.0</version>

  <author name="Roger Lipscombe">
    <info url="http://www.differentpla.net/content/" text="differentpla.net" />
    <logo src="Logo.png" />
  </author>

  <copyright>(C) 2007</copyright>
  <description>Basic Sidebar Gadget</description>
</gadget>
```

A couple of things to note:

*   There's not a lot of room on the right-hand side of the Gadget Gallery, so keep your author name and copyright information short.
*   The information field can handle different display and URL strings. This is useful because it's displayed on the right-hand side of the Gallery, and so should be kept reasonably short.

The `Logo.png` file should be 48 pixels high, and 48 pixels wide. You can use the same file as [the icon used in the gallery]({% post_url 2007/2007-04-23-how-do-i-change-the-icon-used-by-my-gadget %}), but you'll probably put your company logo here instead.
