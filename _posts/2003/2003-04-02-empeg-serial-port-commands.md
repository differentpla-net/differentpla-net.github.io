---
title: "empeg: Serial Port Commands"
date: 2003-04-02T10:29:00.000Z
tags: empeg
x-drupal-nid: 25
x-needs-review: 2003-04-02T10:29:00.000Z
---
You can send the empeg player commands using the serial port. This is a list.

Case is ignored. Some commands do different things, depending on state. Each command usually needs to see LF afterwards.

## Menu Navigation

<table>
<tbody>
<tr>
<th>Command</th>

<th>Action</th>

</tr>

<tr>
<td>M</td>

<td>Open menu/select menu item</td>

</tr>

<tr>
<td>*</td>

<td>Cancel menu</td>

</tr>

<tr>
<td>N</td>

<td>Next menu item</td>

</tr>

<tr>
<td>P</td>

<td>Previous menu item</td>

</tr>

</tbody>

</table>

## Track Navigation

<table>
<tbody>
<tr>
<th>Command</th>

<th>Action</th>

</tr>

<tr>
<td>Space</td>

<td>Play</td>

</tr>

<tr>
<td>C</td>

<td>Play</td>

</tr>

<tr>
<td>W</td>

<td>Pause</td>

</tr>

<tr>
<td>N</td>

<td>Track Forward</td>

</tr>

<tr>
<td>P</td>

<td>Track Back</td>

</tr>

<tr>
<td>F</td>

<td>Start Fast Forward</td>

</tr>

<tr>
<td>B</td>

<td>Start Rewind</td>

</tr>

<tr>
<td>A</td>

<td>Cancel FFWD/REW</td>

</tr>

</tbody>

</table>

## Visuals/Info

<table>
<tbody>
<tr>
<th>Command</th>

<th>Action</th>

</tr>

<tr>
<td>V</td>

<td>Next Visual</td>

</tr>

<tr>
<td>S</td>

<td>Next Info</td>

</tr>

<tr>
<td>D</td>

<td>Next Info</td>

</tr>

</tbody>

</table>

## Quitting/Restarting

<table>
<tbody>
<tr>
<th>Command</th>

<th>Action</th>

</tr>

<tr>
<td>Q</td>

<td>Quit the player.</td>

</tr>

<tr>
<td>R</td>

<td>Restart the player.</td>

</tr>

</tbody>

</table>

## Volume Control

<table>
<tbody>
<tr>
<th>Command</th>

<th>Action</th>

</tr>

<tr>
<td>+</td>

<td>CONTROL_VOLUMEUP</td>

</tr>

<tr>
<td>-</td>

<td>CONTROL_VOLUMEDOWN</td>

</tr>

<tr>
<td>K</td>

<td>CONTROL_LOUDNESSDOWN</td>

</tr>

<tr>
<td>L</td>

<td>CONTROL_LOUDNESSUP</td>

</tr>

</tbody>

</table>

## Input Selector

<table>
<tbody>
<tr>
<th>Command</th>

<th>Action</th>

</tr>

<tr>
<td>[</td>

<td>CONTROL_EMPEG</td>

</tr>

<tr>
<td>]</td>

<td>CONTROL_TUNER</td>

</tr>

<tr>
<td>=</td>

<td>CONTROL_AUX</td>

</tr>

</tbody>

</table>

## Tuner

<table>
<tbody>
<tr>
<th>Command</th>

<th>Action</th>

</tr>

<tr>
<td>]</td>

<td>CONTROL_TUNER</td>

</tr>

<tr>
<td>`</td>

<td>CONTROL_RADIO_STORE</td>

</tr>

<tr>
<td>;</td>

<td>CONTROL_TUNEUP</td>

</tr>

<tr>
<td>.</td>

<td>CONTROL_TUNEDOWN</td>

</tr>

</tbody>

</table>

## Miscellaneous

<table>
<tbody>
<tr>
<th>Command</th>

<th>Action</th>

</tr>

<tr>
<td>*</td>

<td>Mark Track</td>

</tr>

<tr>
<td>0-9</td>

<td>CONTROL_NUMBER0-CONTROL_NUMBER9</td>

</tr>

<tr>
<td>\</td>

<td>CONTROL_SLUMBER</td>

</tr>

<tr>
<td>|</td>

<td>M_COMMAND_CACHE_NOW</td>

</tr>

<tr>
<td>#_fid_</td>

<td>REPLACE</td>

</tr>

<tr>
<td>#_fid_-</td>

<td>ENQUEUE</td>

</tr>

<tr>
<td>#_fid_+</td>

<td>APPEND</td>

</tr>

<tr>
<td>#_fid_!</td>

<td>INSERT</td>

</tr>

<tr>
<td>~</td>

<td>SuggestDiskSpinning</td>

</tr>

<tr>
<td>@</td>

<td>EnsureDiskSpinning</td>

</tr>

<tr>
<td>%</td>

<td>CONTROL_SHUFFLE</td>

</tr>

</tbody>

</table>
