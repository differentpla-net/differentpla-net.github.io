---
title: "How do I change the icon used by my gadget?"
date: 2007-04-23T08:50:36.000Z
tags: sidebar-gadgets
---
When you add a new gadget to the sidebar (by right-clicking and selecting "Add Gadgets..."), Windows Vista displays an icon for it.

![](/images/2007/2007-04-23-how-do-i-change-the-icon-used-by-my-gadget/Add_Gadgets.png)

This icon is controlled by your Gadget.xml file:

```xml
<gadget>
  <icons>
    <icon height="48" width="48" src="GadgetIcon.png" />
  </icons>
</gadget>
```

The icon should be 48 pixels wide and 48 pixels hide. You'll normally use a .PNG file. As you can see from the above screenshot, I took the normal icon and recoloured it (in [Paint.NET](http://www.getpaint.net/)) slightly to demonstrate.
