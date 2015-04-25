---
title: "Things I learnt this week: SetWaitableTimer"
date: 2008-10-25T12:23:56.000Z
x-drupal-nid: 222
x-needs-review: 2008-10-25T12:23:56.000Z
---
SetWaitableTimer doesn't accept NULL for the pDueTime parameter. It returns FALSE; GetLastError() returns ERROR_NOACCESS (998). If you specify zero (i.e. a LARGE_INTEGER containing zero) for pDueTime, the timer is signalled immediately.

A FILETIME unit is 100ns; there are 10,000 of them in a millisecond; there are 10,000,000 of them in a second. Something like this might come in useful:

<pre>const LONGLONG FILETIME_UNITS_PER_MICROSECOND = 10;
const LONGLONG FILETIME_UNITS_PER_MILLISECOND = FILETIME_UNITS_PER_MICROSECOND * 1000;
const LONGLONG FILETIME_UNITS_PER_SECOND = FILETIME_UNITS_PER_MILLISECOND * 1000;

LARGE_INTEGER WaitableTimerDelayFromMilliseconds(LONG milliseconds)
{
   LARGE_INTEGER li;
   li.QuadPart = 0 - (milliseconds * FILETIME_UNITS_PER_MILLISECOND);
   return li;
}</pre>

If you set the timer to auto-reset, it will tick on the period. If you set the timer to manual-reset, it will tick once, and then stay signalled. You can't use ResetEvent to reset a manual-reset timer; it will return FALSE, GetLastError() returns ERROR_INVALID_HANDLE (6).

To reset a manual-reset timer, you need to use SetWaitableTimer, which requires that you pass in another initial delay. If you pass in zero, you get the same behaviour as above.

This means, realistically, that you can't use a periodic timer with a manual-reset timer. Which makes sense, but I've never seen it spelt out anywhere.