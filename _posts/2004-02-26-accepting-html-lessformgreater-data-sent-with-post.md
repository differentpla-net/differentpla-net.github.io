---
title: "Accepting HTML <form> data sent with POST"
date: 2004-02-26T10:06:00.000Z
x-drupal-nid: 62
x-needs-review: 2004-02-26T10:06:00.000Z
---
How to write a CGI script (in Ruby) to accept files uploaded with `POST`.

## ~user/cgi-bin

The first thing that I needed to do in this case was enable CGI processing for the `cgi-bin` directory in my `public_html` directory.

Something like the following added to `/etc/apache/httpd.conf` seemed to do the trick:

<pre><DirectoryMatch ^/home/.*/public_html/cgi-bin>
    AllowOverride None
    Options ExecCGI
    Order allow,deny
    allow from all
    SetHandler cgi-script
</DirectoryMatch></pre>

## The upload form

Before the user can upload a file, they'll need to use an HTML form to specify the file to upload. This, again, is quite easy:

<pre><HTML>
  <HEAD><TITLE>Upload File</TITLE>
    <LINK href="/~roger/upload.css" rel="stylesheet" type="text/css">
  </HEAD>
  <BODY>
    <H1>Upload File</H1>
    <P>
      <FORM METHOD="post" ENCTYPE="multipart/form-data" ACTION="cgi-bin/upload2">
        <FIELDSET><LEGEND>Filename</LEGEND>
          <TABLE>
            <TR><TD>
                <INPUT name="upload_file" size="40" TYPE="file">
            </TD></TR>
            <TR><TD align="right">
                <INPUT TYPE="submit" value="Upload File">
            </TD></TR>
          </TABLE>
        </FIELDSET>
      </FORM>
    </P>
  </BODY>
</HTML>
</pre>

The interesting part in here is the `<FORM METHOD="post" ENCTYPE="multipart/form-data" ACTION="cgi-bin/upload2">`. Normally, when you specify "GET" in `METHOD`, the values in the form are appended to the URL, after a question mark.

When you're sending a file, it'll no longer fit, so we need to use `POST`, which packages the values into the request body. Note that the HTML documentation states that you should only use `GET` for idempotent queries; that is: issuing the same query twice will not have a lasting effect on the state of the universe.

Because we're sending multiple values, we need to specify `ENCTYPE="multipart/form-data"`, which prescribes a certain formatting of the data. The `ACTION` tells the browser which URL to POST the data to. Note that the form is outside the `cgi-bin` directory, otherwise Apache will try to run it, so we've had to specify a relative path to the upload script.

A common tactic is to have the upload script respond differently depending on whether it's being accessed with GET or with POST. Maybe I'll come back to this in a later article.

## Using CGI.rb to handle the POST data

Something like this:

<pre>#!/usr/bin/env ruby

require "cgi"
require "ftools"
require "socket"

def showFileStats(html, tmp)
	html.table {
		html.tr {
			html.td { "Original Filename:" } + 
			html.td { tmp.original_filename }
		} +
		html.tr {
			html.td { "Local Path:" } +
			html.td { tmp.local_path }
		} +
		html.tr {
			html.td { "Local Path:" } +
			html.td { tmp.content_type }
		} +
		html.tr {
			html.td { "File Size:" } +
			html.td { tmp.stat.size.to_s }
		}
	}
end

def showFileContents(html, tmp)
	html.pre { tmp.read }
end

def copyFile(html, tmp)
	toName = "/tmp/%d.M%dP%d.%s" %
		[ Time.now.to_i, Time.now.usec, $$, Socket.gethostname ]
	if File.syscopy(tmp.local_path, toName) then
		html.p { "File " + html.code { tmp.local_path } + 
			" successfully copied to " + html.code { toName } }
	else
		html.p { "Failed to copy " + html.code { tmp.local_path } +
			" to " + html.code { toName } }
	end
end

def showHead(html)
	html.head {
		html.title { "Uploaded File" } +
		html.link("rel"=>"stylesheet",
			"type"=>"text/css",
			"href"=>"/upload.css")
	}
end

def showBody(query, html)
	html.body {
		value = query['upload_file'][0]
		if value
			html.h1 { "Uploaded File" } +
			showFileStats(html, value) +
			html.hr +
#			showFileContents(html, value) +
#			html.hr +
			copyFile(html, value)
		else
			html.h1 { "Error" } +
			html.p { "You must specify a filename." } +
			html.p { "Press the Back button " +
				"on your browser and try again" }
		end
	}
end

query = CGI.new
html = CGI.new("html4Tr")
html.out {
	CGI.pretty (
		html.html {
			showHead(html) +
			showBody(query, html)
		}
	)
}</pre>

This script accepts the file data attached to the `upload_file` field and saves it in `/tmp`, generating a unique filename for it.

## Security Considerations

There's nothing in this script stopping anyone from uploading anything to your `/tmp` directory. They can easily cause a denial of service attack with this. Resolving this is outside the scope of this article.