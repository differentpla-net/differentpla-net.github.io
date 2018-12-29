---
title: "Rio Receiver: Layout Definitions"
date: 2002-01-22T18:06:00.000Z
---
## Introduction

Inspired by the stirling effort that David Schuetz has put into deciphering the layout definitions for the Rio Receiver (see [here](http://www.dasnet.org/David/Rio/Layout.html)), I've decided to expand on his work.

Obviously, I'm at a distinct advantage, 'cos I get to look at the source code (and designed/wrote some of it, too).

**Note:** All of the information contained herein is subject to the whims of the SONICblue development team, should they decide to revamp this for a later version of the product.

## Getting the list of Layouts

The layout definitions are stored on the server. By default, these are in the `C:\Program Files\Audio Receiver\Layout` directory. This location is stored in the `HKEY_LOCAL_MACHINE\SOFTWARE\Diamond Multimedia\Audio Receiver Manager\1.0\HTTP` registry key.

In order to get a list of layouts supported, the Receiver first asks the server for `/layout/_language_/index`. The language is set to `en_UK` at build time. The server hands over a copy of `_layout-directory_\_language_\index`, which is displayed on the screen of the Receiver.

This file contains a list of the layout names, and their absolute locations on the server. This was done so that multiple languages could share the same definition files, and only the index file would need to be changed. The file looks like this:

<div class="snippet"> `<pre>/layout/en_UK/all_info=All Info
/layout/en_UK/remaining=Remaining Time
/layout/en_UK/scope=Scope View
/layout/en_UK/inverse_scope=Inverse Scope
/layout/en_UK/big_title=Big Title</pre>` </div>

Once you've picked a layout from the list, the Receiver goes and grabs the file from the location listed in the index.

## Format of the Layout File

The file consists of a sequence of information blocks, detailing each item (or Widget) that will be drawn on the screen. It's arranged like this:

<div class="snippet"> `<pre>struct WidgetData {
    INT32 id;
    INT32 left;
    INT32 top;
    INT32 right;
    INT32 bottom;
    INT32 len;
    char payload_data[len];
};</pre>` </div>

All integers are little-endian. I note, however -- from looking at a later version of the source code -- that the most-significant 3 bytes of `id` and `len` should be considered reserved, i.e. they should be expressed as:
<pre>struct {
    BYTE id_or_len;
    BYTE reserved1;
    BYTE reserved2;
    BYTE reserved3;
};</pre>

Whether that particularly evolutionary offshoot will go anywhere remains to be seen, however.
## Layout IDs

<table>
<tbody>
<tr>
<th>ID</th>

<th>Description</th>

</tr>

<tr>
<td>0</td>

<td>Blank</td>

</tr>

<tr>
<td>1</td>

<td>Text -- see "Text Widgets"</td>

</tr>

<tr>
<td>2</td>

<td>Track Info -- see "Text Widgets"</td>

</tr>

<tr>
<td>3</td>

<td>Time -- see "Text Widgets"</td>

</tr>

<tr>
<td>4</td>

<td>Play State -- see "Text Widgets"</td>

</tr>

<tr>
<td>5</td>

<td>Bitmap</td>

</tr>

<tr>
<td>6</td>

<td>Rectangle</td>

</tr>

<tr>
<td>7</td>

<td>Volume</td>

</tr>

<tr>
<td>8</td>

<td>Scope -- see "Scope Widget"</td>

</tr>

<tr>
<td>9</td>

<td>Status -- see "Text Widgets"</td>

</tr>

</tbody>

</table>

## Text Widgets

The various payloads look like this:

<div class="snippet"> `<pre>struct TextWidgetData {
    BYTE justify, style, font_index;
    BYTE text_len;
    char text[text_len];
};</pre>` </div>

<table>
<tbody>
<tr valign="top">
<td>
### justify

<table>
<tbody>
<tr>
<td>0</td>

<td>left</td>

</tr>

<tr>
<td>1</td>

<td>centre</td>

</tr>

<tr>
<td>2</td>

<td>right</td>

</tr>

</tbody>

</table>

</td>

<td>
### style

<table>
<tbody>
<tr>
<td>0</td>

<td>normal</td>

</tr>

<tr>
<td>1</td>

<td>inverse</td>

</tr>

<tr>
<td>2</td>

<td>background</td>

</tr>

<tr>
<td>3</td>

<td>transparent</td>

</tr>

</tbody>

</table>

</td>

<td>
### font_index

<table>
<tbody>
<tr>
<td>0</td>

<td>medium</td>

</tr>

<tr>
<td>1</td>

<td>small</td>

</tr>

<tr>
<td>2</td>

<td>graphics</td>

</tr>

<tr>
<td>3</td>

<td>timecode</td>

</tr>

<tr>
<td>4</td>

<td>large</td>

</tr>

</tbody>

</table>

</td>

</tr>

</tbody>

</table>

<div class="snippet"> `<pre>struct TrackInfoWidgetData {
    BYTE justify, style, font_index;
    BYTE which;
};</pre>` </div>

### which

<table>
<tbody>
<tr>
<td>0</td>

<td>title</td>

</tr>

<tr>
<td>1</td>

<td>artist</td>

</tr>

<tr>
<td>2</td>

<td>source</td>

</tr>

<tr>
<td>3</td>

<td>source_year</td>

</tr>

<tr>
<td>4</td>

<td>year</td>

</tr>

<tr>
<td>5</td>

<td>bitrate</td>

</tr>

<tr>
<td>6</td>

<td>playlist</td>

</tr>

<tr>
<td>7</td>

<td>codec</td>

</tr>

<tr>
<td>8</td>

<td>duration</td>

</tr>

</tbody>

</table>

<div class="snippet"> `<pre>struct TimeWidgetData {
    BYTE justify, style, font_index;
    BYTE type;
};</pre>` </div>

### type

<table>
<tbody>
<tr>
<td>0</td>

<td>elapsed</td>

</tr>

<tr>
<td>1</td>

<td>remaining</td>

</tr>

</tbody>

</table>

<div class="snippet"> `<pre>struct PlaystateWidgetData {
    BYTE justify, style, font_index;
};</pre>` </div>

<div class="snippet"> `<pre>struct StatusWidgetData {
    BYTE justify, style;
    BYTE cond;
};</pre>` </div>

### cond

<table>
<tbody>
<tr>
<td>0</td>

<td>mute</td>

</tr>

<tr>
<td>1</td>

<td>repeat</td>

</tr>

<tr>
<td>2</td>

<td>random</td>

</tr>

</tbody>

</table>

## Bitmap Widget

<div class="snippet"> `<pre>struct BitmapWidgetData {
    INT32 bpp;
    INT32 width, height;
    BYTE data[];
};</pre>` </div>

`bpp` is bits-per-pixel, and must be 4\. `width` and `height` are in pixels. As far as I can tell, the bitmap data is stored as rows of 4-bit colour values.

## Scope Widget

There's no payload in this one.
