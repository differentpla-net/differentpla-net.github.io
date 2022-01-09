---
title: "Rio Receiver: Layout Definitions"
date: 2002-01-22T18:06:00.000Z
redirect_from: /node/view/12
tags: rio-receiver
---
## Introduction

Inspired by the stirling effort that David Schuetz has put into deciphering the layout definitions for the Rio Receiver (see [here](http://www.dasnet.org/David/Rio/Layout.html)), I've decided to expand on his work.

Obviously, I'm at a distinct advantage, 'cos I get to look at the source code (and designed/wrote some of it, too).

**Note:** All of the information contained herein is subject to the whims of the SONICblue development team, should they decide to revamp this for a later version of the product.

## Getting the list of Layouts

The layout definitions are stored on the server. By default, these are in the `C:\Program Files\Audio Receiver\Layout` directory. This location is stored in the `HKEY_LOCAL_MACHINE\SOFTWARE\Diamond Multimedia\Audio Receiver Manager\1.0\HTTP` registry key.

In order to get a list of layouts supported, the Receiver first asks the server for `/layout/<language>/index`. The language is set to `en_UK` at build time. The server hands over a copy of `<layout-directory>\<language>\index`, which is displayed on the screen of the Receiver.

This file contains a list of the layout names, and their absolute locations on the server. This was done so that multiple languages could share the same definition files, and only the index file would need to be changed. The file looks like this:

```
/layout/en_UK/all_info=All Info
/layout/en_UK/remaining=Remaining Time
/layout/en_UK/scope=Scope View
/layout/en_UK/inverse_scope=Inverse Scope
/layout/en_UK/big_title=Big Title
```

Once you've picked a layout from the list, the Receiver goes and grabs the file from the location listed in the index.

## Format of the Layout File

The file consists of a sequence of information blocks, detailing each item (or Widget) that will be drawn on the screen. It's arranged like this:

```c
struct WidgetData {
    INT32 id;
    INT32 left;
    INT32 top;
    INT32 right;
    INT32 bottom;
    INT32 len;
    char payload_data[len];
};
```

All integers are little-endian. I note, however -- from looking at a later version of the source code -- that the most-significant 3 bytes of `id` and `len` should be considered reserved, i.e. they should be expressed as:

```c
struct {
    BYTE id_or_len;
    BYTE reserved1;
    BYTE reserved2;
    BYTE reserved3;
};
```

Whether that particularly evolutionary offshoot will go anywhere remains to be seen, however.

## Layout IDs

| ID | Description |
|----|-------------|
| 0  | Blank |
| 1  | Text -- see "Text Widgets" |
| 2  | Track Info -- see "Text Widgets" |
| 3  | Time -- see "Text Widgets" |
| 4  | Play State -- see "Text Widgets" |
| 5  | Bitmap |
| 6  | Rectangle |
| 7  | Volume |
| 8  | Scope -- see "Scope Widget" |
| 9  | Status -- see "Text Widgets" |

## Text Widgets

The various payloads look like this:

```c
struct TextWidgetData {
    BYTE justify, style, font_index;
    BYTE text_len;
    char text[text_len];
};
```

### justify

| ID | Meaning |
|---|------|
| 0 | left |
| 1 | centre |
| 2 | right |

### style

| ID | Meaning |
|---|------|
| 0 | normal |
| 1 | inverse |
| 2 | background |
| 3 | transparent |

### font_index

| ID | Meaning |
|---|------|
| 0 | medium |
| 1 | small |
| 2 | graphics |
| 3 | timecode |
| 4 | large |

```c
struct TrackInfoWidgetData {
    BYTE justify, style, font_index;
    BYTE which;
};
```

### which

| ID | Meaning |
|---|------|
| 0 | title |
| 1 | artist |
| 2 | source |
| 3 | source_year |
| 4 | year |
| 5 | bitrate |
| 6 | playlist |
| 7 | codec |
| 8 | duration |

```c
struct TimeWidgetData {
    BYTE justify, style, font_index;
    BYTE type;
};
```

### type

| ID | Meaning |
|---|------|
| 0 | elapsed |
| 1 | remaining |

```c
struct PlaystateWidgetData {
    BYTE justify, style, font_index;
};
```

```c
struct StatusWidgetData {
    BYTE justify, style;
    BYTE cond;
};
```

### cond

| ID | Meaning |
|---|------|
| 0 | mute |
| 1 | repeat |
| 2 | random |

## Bitmap Widget

```c
struct BitmapWidgetData {
    INT32 bpp;
    INT32 width, height;
    BYTE data[];
};
```

`bpp` is bits-per-pixel, and must be 4. `width` and `height` are in pixels. As far as I can tell, the bitmap data is stored as rows of 4-bit colour values.

## Scope Widget

There's no payload in this one.
