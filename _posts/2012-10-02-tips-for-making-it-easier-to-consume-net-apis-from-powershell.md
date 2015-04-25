---
title: "Tips for making it easier to consume .NET APIs from PowerShell"
date: 2012-10-02T12:45:26.000Z
x-drupal-nid: 282
x-needs-review: 2012-10-02T12:45:26.000Z
---
### Prefer <tt>enum</tt> types over constants or properties

If you use an enum, PowerShell can implicitly convert the value:

<pre>[System.Environment]::GetFolderPath('LocalApplicationData')</pre>

If you use constants or static properties, you need a load more typing.

### Prefer synchronous methods to event-based asynchronous methods

PowerShell 2.0 can subscribe to .NET events, and it's pretty powerful. It's just fairly verbose. Given the choice between a synchronous (blocking) method and the [Event-based Asynchronous Pattern](http://msdn.microsoft.com/en-us/library/wewwczdw.aspx), provide a synchronous interface.

Of course, you could provide both, but it's all about <tt>await</tt> these days anyway, which PowerShell doesn't support.

### Don't use explicit interface implementation

It's [not accessible from PowerShell](https://connect.microsoft.com/feedback/ViewFeedback.aspx?FeedbackID=249840&SiteID=99).