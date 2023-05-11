---
title: "Erlang: Network Protocol Notes"
date: 2023-04-20T13:02:00.000Z
tags: erlang
---

## On the difference between binary and text-based network protocols.

Note that a lot of discussion about network protocols also applies to file formats.

Give an example of a text-based protocol, such as HTTP/1.1, because:
- It's relatively human-readable.
- It demonstrates that numbers take up too much space.
- It demonstrates that, for, e.g., multipart forms, we need to escape binary content.
  - or we need a Content-Type header.
- Tends to lean towards terminators, rather than length prefixes. Though note the Content-Length header in HTTP/1.1.

The advantage of binary protocols is that they are, generally-speaking, more compact.

The advantage of text-based protocols is that they are, generally-speaking, human-readable.

They allow the use of already-existing tools, such as telnet, netcat, etc. Other tools in this space?

Disadvantages: less compact; need to worry about character sets, though UTF-8 generally deals with that now. Refer to
Dylan's presentation about character sets, maybe: https://www.youtube.com/results?search_query=dylan+beattie+plain+text.

UTF-8 is endian-independent, because it specifies the encoding in octets, and defines the order in which they are
transmitted and received.

General networking:

- Difference between UDP and TCP.
- Byte order.
- The need to collect the entire message before attempting to parse it.
- Motivated by: messages larger than the MTU are split.
  - Are there any tools to deliberately introduce splits?
    - If you've got a length prefix, it probably fits in the packet, so you don't need to deal with it being split in two. You _should_.
    - If the sender catenates a bunch of messages, at _that_ point, the size prefix might straddle the packet boundary. They might do this because of Nagle, e.g.
- How to detect size?
  - Length prefix.
    - Refer to Heartbleed, etc., here>
  - Terminator, particularly in text-based formats.
  - You know what message it is (based on magic number), so you know how large it is.
    - Doesn't work for compatibility -- if you see a message that you don't recognise, you don't know how to skip it, e.g.
    - On the other hand, if you don't recognise a message, that would be bad as well, right?
    - Doesn't allow variable-length messages, which means things like strings are constrained to fixed sizes (truncation is bad) or padded (waste of space). That waste of space isn't so important in memory, where cache line alignment stuff might dominate, but it's important on the network, where space is usually at a premium.
- Compression.
- Version negotiation.
  - You don't want to send messages that the other end doesn't understand.
  - Upon connecting, the server could advertise its protocol version as the first message.
  - The client could send its protocol version to the server as _its_ first message.
  - Between them, they can figure out which messages to send. If either end is unwilling, it can just disconnect.
  - Or, Kafka: the messages are individually versioned, and the client asks the server for a list of min/max for each message and is then expected to constrain itself to just that.
  - Or, if you're using some other kind of serialization, such as protobuf, then that's kinda included in how you'd version those anyway. I think.
- Schema registries vs. self-describing formats.
- Encoding: text vs binary. JSON, protobufs, Thrift, ASN1, XML, BERT, etc., etc., etc.
- Endianness.
  - Big-endian.
  - Little-endian.
  - Receiver-makes-right.
- Varint encoding.
  - Relationship with UTF-8 encoding.
- Checksums - don't bother for network protocols. The layer below you should be doing that.
- Dealing with errors - just die, or return an error.
- Serialization options.
  - All kinds of stuff to talk about here.
- Network Protocol Menagerie...
- Is there a survey of network protocols anywhere, in terms of endianness, serialization, etc.?
  - I'm sure I asked this Q on Stack overflow or Twitter at some point...?
  - HTTP, XDR (Sun RPC), NDR (DCE RPC, DCOM), protobuf, yamux. What other network protocols are there? TCP and IP
    itself? FTP. TFTP, BOOTP, etc. MySQL, Postgres, Kafka, T-SQL (or whatever the network transport is called, I don't
    remember).
- Timeouts; what if a client connects and then never sends anything? What if it connects and then takes _forever_ to
  send a message? Clients being excessively spammy...? Most of these are probably best dealt with by a security
  appliance thing, maybe.
- Message size? If the sender includes a message size prefix, the receiver needs to allocate some memory. Should we
  enforce a maximum message size? What if the message is actually a file-write? Then we're just streaming the content to
  disk, and it doesn't need to take up any memory.
- Correlation IDs.
  - use in Kafka means you need to know what the outbound message was in order to know how to parse the response, which
    means you need to track the correlation IDs.
  - use for associating requests and replies.
  - use for associating subscribes and notifies.
- Request/Reply vs. Pub/Sub.
- Stream vs. message, and how one can be expressed in terms of the other.
- Multiplexing.
- Does the response have an ID tag in it?
- Is it serialize/deserialize or encode/decode or marshal/unmarshal (or marshall/unmarshall).
- Otherwise: need the correlation ID.
- Pipelining: can we have multiple messages in flight at once?
- Even if you do, you don't really need the correlation ID. You can correlate the responses to the requests by order.
- But if the messages can _overlap_, _then_ you need the correlation ID.
- Historical: CTI stuff at Aspect.
  - request/response (solicited) and unsolicited messages. You set up a listen/subscription/whatever, and you get back
    an ID for that. Messages relevant to that have that ID in them, so you can correlate them.
  - would you do it that way these days? Maybe. Or maybe you'd just open multiple sockets.
- Bitflags.
- Text encoding. Assume UTF-8, but not necessarily. MIME multipart.
- How to navigate through firewalls.
- FTP active vs. passive, etc.
- Using the same channel for control and data.
- Correlation ID: start at zero and increment monotonically? Start at a different, fixed, number and increment monotonically? Start at a random number and increment monotonically? Completely PRNG each time? Completely CRNG each time?
- Inspection:
  - Client IDs
- TLS
- Authentication is out of scope for now, but later, maybe:
  - SASL
  - Client Certs
- Naming: length vs size vs count
- Inactivity timeouts. Heartbeating.
- Something about load-shedding. Gracious disconnects.
- Retry intervals, exponential backoff.
- Use of connection pools.

Erlang considerations; some other more general language things:

- Active mode.
- Maybe some basics about binary pattern matching and why it's awesome.
- The need to collect the entire message before attempting to parse it.
  - ferd's post about avoiding catenating the buffer on every packet until you need to. https://cohost.org/mononcqc/post/1128460-on-the-hunt-for-a-bu

- Using a separate serde/codec from the network piece.
- The use of ranch for client and server.
  - Why _did_ I do that?
- While we're talking about Erlang, what are common implementation patterns, in terms of modules, processes, gen_server
  vs. gen_statem, etc.? Encoder and decoder in the same module? They're generally symmetric in terms of primitives, but
  not necessarily in terms of messages. That is: the client encodes requests and decodes replies; the server decodes
  requests and encodes replies. So there's a symmetry for each request/reply pair, but an asymmetry in terms of who
  needs which. So do you just have a "codec" module, or separate "encoder" and "decoder" modules, or do you have all 4
  -- {request|reply}_{encoder|decoder}? There's not necessarily any right answer. It depends, sure, but on what?
- Decoding patterns in C:
  - explicitly unmarshalling the whole thing properly.
  - vs. reading it into a struct and then making it right.
- What _is_ the cross-module call penalty in Erlang these days anyway?
  - https://www.erlang.org/doc/efficiency_guide/functions.html#function-calls says "Calls to local or external functions (foo(), m:foo()) are the fastest calls."

```erlang
handle_info(
    {tcp, Socket, Data}, State = #state{socket = Socket, buffer = Buffer}
) ->
    % TODO: This needs to be in a separate module, for ease of testing.
    case <<Buffer/binary, Data/binary>> of
        % TODO: DoS protection against messages that are too long.
        <<Length:32/big, Message:Length/binary, Rest/binary>> ->
            ?LOG_DEBUG("Got message ~p", [Message]),
            {noreply, State#state{buffer = Rest}};
        NewBuffer ->
            {noreply, State#state{buffer = NewBuffer}}
    end.
```
