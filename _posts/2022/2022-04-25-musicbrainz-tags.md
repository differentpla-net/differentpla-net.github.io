---
title: "MusicBrainz tags: MUSICBRAINZ_ALBUMID or MusicBrainz Album Id"
date: 2022-04-25T14:13:00Z
tags: musicbrainz
---

I noticed that some of my music collection doesn't have the correct album art. While investigating, I discovered an
unrelated problem with the MusicBrainz tags.

Because I'm old-school, I rip physical CDs to .FLAC files, tag the FLAC files with [MusicBrainz
Picard](https://picard.musicbrainz.org/), and then transcode those to .MP3 files using a `Rakefile` and `ffmpeg`. I
generally do this on my Linux box, and then use [Syncthing](https://syncthing.net/) to ensure that the FLAC files are
pushed to my Synology NAS, and the MP3 files are replicated as relevant.

Earlier today, on my Windows laptop, I noticed that some of my MP3 files were missing album artwork -- and I'd forgotten
that they _were_ MP3 files -- so I opened the `Music` folder in Picard and was puzzled that almost none of my music was
correctly grouped into albums.

Using the "Lookup" function on an arbitrary file in Picard correctly identified the music track, but there was a
discrepancy: The track had `MUSICBRAINZ_ALBUMID` tags, and Picard was suggesting the addition of `MusicBrainz Album Id`
tags.

Some quick searching on the internet later, and I found this:
<https://community.metabrainz.org/t/what-tag-names-are-correct/16998>, which pointed me to
<https://picard-docs.musicbrainz.org/en/appendices/tag_mapping.html>, which lays out the cause:

- For MP3 files (i.e. ID3v2 tags), Picard uses `TXXX: MusicBrainz Album Id` tags.
- For FLAC files (i.e. Vorbis tags), Picard uses `MUSICBRAINZ_ALBUMID` tags.

My transcoding script -- which is [here](https://gist.github.com/rlipscombe/6f1a376b40bf045befbd2a99ae5a4512) -- just
uses `ffmpeg` to transcode each FLAC file into an MP3 file. It would seem that ffmpeg just converts the Vorbis tags to
`TXXX` ID3v2 tags. Which makes sense -- why would it know that MusicBrainz uses different names?

There doesn't _seem_ to be a way to fine-tune the behaviour of the `-map_metadata` option to `ffmpeg` (which is
apparently implied by default, since I don't use it explicitly), so my options seem to be as follows:

1. Accept the situation; my MP3 files won't be properly grouped by album, etc., as far as MusicBrainz is concerned.
2. Use something other than ffmpeg to transcode the files.
3. Have the Rakefile make a second pass to fix up the MusicBrainz tags.

For now, I'll just live with it.
