---
title: "More unplanned downtime"
date: 2009-07-04T14:54:37.000Z
x-drupal-nid: 236
x-needs-review: 2009-07-04T14:54:37.000Z
---
The hosting outfit that I've been using for differentpla.net is cheap for a reason: they're not very good.

At the end of February, I managed to hose my VPS host. This, fair enough, was my fault. Where I started to run into problems was when I asked my hosting company to restore a backup. The backup was corrupted. Ordinarily, this wouldn't be a problem: I had a fairly recent backup stored locally, so once the box was restored to the point that I could log in, I just restored from that.

Shortly afterwards, though, freaky things started to happen: files all over the disk were getting cross-linked with each-other. The MySQL database that runs this website got replaced with an XML file from the Ruby documentation. Not good.

I asked the support department at my hosting company to run fsck. "Oh no," they said, "we can't do that; it'll mean taking down the VPS host". Great. So now I'm left with a website that limps along, keeping my fingers crossed that nothing else gets overwritten. However, I've got a day job and a young son, so I don't have a lot of time to worry about it.

Then, of course, the value of the pound collapsed against the dollar, meaning that my hosting costs doubled. Suddenly, cheap isn't so cheap anymore.

But the straw that broke the camel's back is the fact that, for 5 out of the last 7 days, I've been unable to reach my VPS.

Screw that noise. I've moved my email to Google Apps and I've moved my website back to the other end of my DSL line at home.

Unfortunately, there might still be a few things screwy with it. I'll try to fix those when I get a chance. If you spot anything, leave a comment on the relevant page.