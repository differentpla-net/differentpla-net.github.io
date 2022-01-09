---
title: "How do I add a settings page to my gadget?"
date: 2007-04-23T09:47:28.000Z
---
Some gadgets allow the user to configure extra settings.

![](/content/system/files/images/gadget_settings.png)

According to the Vista design guidelines, you should avoid doing this if possible, and you should always choose good defaults, rather than force the user to configure anything.

To add the settings button (the spanner icon), you need to add the following `<script>` block to the `<head>` section of your Gadget.htm file:

```html
<script>
    System.Gadget.settingsUI = "Settings.htm";
    System.Gadget.onSettingsClosed = settingsClosed;

    function settingsClosed(event)
    {
        if (event.closeAction == event.Action.commit)
        {
        }
    }
</script>
```

The `settingsClosed` function is called when the user closes the settings window. You'll probably use this to refresh the gadget's display.

For more information, check out [this page](http://msdn2.microsoft.com/en-us/library/ms723694.aspx#_sidebar_gdo_08_GadgetSettings).
