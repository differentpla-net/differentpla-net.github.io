---
title: "Moving Message Sign"
date: 2001-06-01T10:41:00.000Z
redirect_from: /content/2001/06/moving-message-sign
---
A couple of weeks ago, we got an LED message board at work. They had them on offer at Maplin (if I recall correctly). Anyway, the manual with the board is singularly useless. It's labelled "Moving Message Sign", and has a picture of the display with "Taiwan Kingpul" on it.

It comes with a serial connector. Unfortunately, you don't seem to be able to control the LED sequencing yourself, but you can upload messages to it, using the supplied software.

Unfortunately, the supplied software isn't that good - it's a 16-bit Windows program. We'd like to run something under Linux to control it. So with the judicious application of a copy of PortMon from [www.sysinternals.com](http://www.sysinternals.com/), we found out how it works.

The connection settings are 2400,N,8,1.

To program it, get it either scrolling a message, or in the program editor, then send it the new message.

The message should be preceded by a preamble consisting of 0xAA,0xAA,0xAA,0xAA,0xAA,0xBB, and terminated by a single 0x80 byte.

Messages are almost in ASCII.

<table border="1">
<tbody>
<tr>
<th>Hex</th>

<th>Character</th>

</tr>

<tr>
<td>0x03</td>

<td>À</td>

</tr>

<tr>
<td>0x06</td>

<td>£</td>

</tr>

<tr>
<td>0x07</td>

<td>¤</td>

</tr>

<tr>
<td>0x08</td>

<td>Ω</td>

</tr>

<tr>
<td>0x0F</td>

<td>Ö</td>

</tr>

<tr>
<td>0x10</td>

<td>φ</td>

</tr>

<tr>
<td>0x13</td>

<td>à</td>

</tr>

<tr>
<td>0x14</td>

<td>§</td>

</tr>

<tr>
<td>0x16</td>

<td>æ</td>

</tr>

<tr>
<td>0x17</td>

<td>Σ</td>

</tr>

<tr>
<td>0x18</td>

<td>wineglass</td>

</tr>

<tr>
<td>0x1F</td>

<td>ö</td>

</tr>

<tr>
<td>0x20</td>

<td>colon (swapped with space)</td>

</tr>

<tr>
<td>0x21 to 0x39</td>

<td>(same as ASCII)</td>

</tr>

<tr>
<td>0x3A</td>

<td>space (swapped with colon)</td>

</tr>

<tr>
<td>0x3B</td>

<td>Φ</td>

</tr>

<tr>
<td>0x3C</td>

<td>Æ</td>

</tr>

<tr>
<td>0x3D to 0x5A</td>

<td>(same as ASCII)</td>

</tr>

<tr>
<td>0x5B</td>

<td>Ü</td>

</tr>

<tr>
<td>0x5D</td>

<td>è</td>

</tr>

<tr>
<td>0x5F</td>

<td>less-than</td>

</tr>

<tr>
<td>0x60</td>

<td>É</td>

</tr>

<tr>
<td>0x7B</td>

<td>semicolon</td>

</tr>

<tr>
<td>0x7C</td>

<td>Ñ</td>

</tr>

<tr>
<td>0x7D</td>

<td>ñ</td>

</tr>

<tr>
<td>0x7E</td>

<td>Ä</td>

</tr>

<tr>
<td>0x7F</td>

<td>ä</td>

</tr>

<tr>
<td>0x80</td>

<td>end</td>

</tr>

<tr>
<td>0x81</td>

<td>left</td>

</tr>

<tr>
<td>0x82</td>

<td>right</td>

</tr>

<tr>
<td>0x83</td>

<td>up</td>

</tr>

<tr>
<td>0x84</td>

<td>down</td>

</tr>

<tr>
<td>0x85</td>

<td>jump</td>

</tr>

<tr>
<td>0x86</td>

<td>open</td>

</tr>

<tr>
<td>0x87</td>

<td>close</td>

</tr>

<tr>
<td>0x88</td>

<td>flash</td>

</tr>

<tr>
<td>0x89</td>

<td>flshg</td>

</tr>

<tr>
<td>0x8A</td>

<td>doff</td>

</tr>

<tr>
<td>0x8B</td>

<td>big</td>

</tr>

<tr>
<td>0x8C</td>

<td>clear</td>

</tr>

<tr>
<td>0x8D</td>

<td>speed*</td>

</tr>

<tr>
<td>0x8E</td>

<td>random</td>

</tr>

<tr>
<td>0x8F</td>

<td>wait*</td>

</tr>

</tbody>

</table>

* speed and wait should be immediately followed by a literal numeral (i.e. 0x31 to 0x39), dictating how fast, or how long.
 There are a bunch more graphical characters, which do things like put a clock up. I don't know the codes for those.
**Update:** I no longer have access to this display, so I can't help you beyond what's published here.
