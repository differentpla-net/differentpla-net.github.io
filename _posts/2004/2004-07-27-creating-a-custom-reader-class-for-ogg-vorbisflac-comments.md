---
title: "Creating a Custom Reader Class for Ogg Vorbis/FLAC Comments"
date: 2004-07-27T15:49:00.000Z
---
In Dino Esposito's "Cutting Edge" column in [MSDN Magazine](http://msdn.microsoft.com/msdnmag/issues/02/08/CuttingEdge/default.aspx), he talks about the various readers (`BinaryReader`, `XmlReader`, etc.) available in .NET.

At the end of the column, he says a couple of things about how to design and implement a custom reader.

Here's how I went about implementing a custom reader for reading [Vorbis comments](http://www.xiph.org/ogg/vorbis/doc/v-comment.html) from [FLAC](http://flac.sourceforge.net/) files.

The first thing that we need to do is decide on what methods and properties the custom reader should support. We're not tied to a specific interface, because `Reader` is a .NET pattern, rather than a specified type.

As it's a reader, we'll probably want to construct it with a stream. Most of the other readers defined in .NET allow this, so it seems like a good idea.

<pre>public class VorbisCommentReader
{
    public VorbisCommentReader(Stream stream)
    {
    }
}</pre>

What about the rest of the methods and properties? Vorbis comments begin with a vendor ID, which is followed by a sequence of `tag=value` comments.

It seems sensible, then, to add a `VendorString` property to our class:

<pre>    public string VendorString
    {
        get { return this.vendorString; }
    }</pre>

## Interface Options

As for the `tag=value` comments, we could just suck them up into a collection, and iterate over them with `foreach`, like this:

<pre>VorbisCommentReader r = new VorbisCommentReader(stream);
foreach (UserComment uc in r.UserComments)
{
    string fieldName = uc.FieldName;
    string fieldValue = uc.FieldValue;
}</pre>

That's not bad. It requires that we load all the comments at once, unless we want to do something a little more complicated in our `IEnumerator` implementation.

We could do this:

<pre>VorbisCommentReader r = new VorbisCommentReader(stream);
for (int i = 0; i < r.ReadCommentCount(); ++i)
{
    string fieldName = r.ReadFieldName();
    string fieldValue = r.ReadFieldValue();
}
</pre>

That's not bad, but the user might accidentally reverse the two calls, which makes it more fragile than it could be.
For something like this, I've started opting for a `DataReader`-style interface:

<pre>VorbisCommentReader r = new VorbisCommentReader(stream);
while (r.Read())
{
    string fieldName = r.GetFieldName();
    string fieldValue = r.GetFieldValue();
}</pre>

## Implementing the constructor

The constructor will assume that the stream is correctly positioned at the start of the block. Because `Stream` only defines some very basic reading methods (`Read`, which reads an array of bytes; `ReadByte` which reads a single byte), we'll need to use a `BinaryReader` internally:

<pre>public VorbisCommentReader(Stream stream)
{
    this.binaryReader = new BinaryReader(stream);
    int vendorLength = binaryReader.ReadInt32();

    // The next thing is a UTF8 string.
    byte[] vendorBytes =
        binaryReader.ReadBytes(vendorLength);
    this.vendorString =
        Encoding.UTF8.GetString(vendorBytes);

    this.userCommentListLength = binaryReader.ReadInt32();
    this.userCommentListIndex = 0;
}</pre>

We read the various fixed values from the comment block, and reset our list index.

## Implementing Read

We can then implement the `Read` method like this:

<pre>    public bool Read()
    {
        if (userCommentListIndex < userCommentListLength)
        {
            int userCommentLength = binaryReader.ReadInt32();

            byte[] userCommentBytes =
                binaryReader.ReadBytes(userCommentLength);
            string userCommentString =
                Encoding.UTF8.GetString(userCommentBytes);

            int pos = userCommentString.IndexOf('=');
            this.fieldName =
                userCommentString.Substring(0, pos);
            this.fieldValue =
                userCommentString.Substring(pos + 1);

            ++this.userCommentListIndex;

            return true;
        }

        return false;
    }</pre>

If we're being nitpicky, this isn't completely correct. Only the field value (after the =) is allowed to be UTF8, but since 7-bit field names are a subset of UTF8, we can probably get away with it.
## More implementation

With this in place, the other methods are a doddle:

<pre>    public string GetFieldName()
    {
        return this.fieldName;
    }

    public string GetFieldValue()
    {
        return this.fieldValue;
    }</pre>

## Conclusion

Writing custom reader objects is easy, and it makes your code feel a bit more idiomatic; it feels like the other .NET readers.

I've also implemented a custom reader to go with this one that allows reading of the metadata blocks in a FLAC file. Source code for both will appear at some point.