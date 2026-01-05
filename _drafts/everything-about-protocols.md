Everything I know about network protocols

- Request, Response
- Asynchronous notifications. Thinking about Aspect CTI interface here, where you could subscribe to stuff, and it would send you messages about events.
- Pipelining. Can I have more than one message pending at once? Usually yes, unless the protocol is super-weird (like you _need_ a response from a previous message to build the next one), but mostly people don't do that.

- Some common protocols: break them down: thinking Redis' RESP, Kafka, etc.
- Serialization formats: Protobufs, BERT, etc.

- Base64 encoding.
- ASCII armour.
- multi-part binaries.
- Compression.

- Point out that other than the back-and-forth thing, most of these are serialization concerns that apply to files as well.
- OTOH, network protocols can't require you to read to the end before being able to parse (which is why ID3v1 is stupid).
- Checksums. Hashes.
- TLS, etc.
- Certificates, digital signatures, etc.

- Something about TCP needing framing. UDP kinda doesn't. That's on top of the "usual" TCP vs UDP differences.
- There's more than just TCP and UDP running on IP.

- Text vs. binary

- Headers, Keys, Values

- Endianness

- Length prefixes: are they inclusive of themselves or not?

- varints

- strings: length prefixed, fixed size, null terminated.
- How do we do arrays?

- Correlation IDs
- Are the packets self-describing?

- More particularly, do I need to see a previous packet to figure out the format of this one? I'm looking at you, Kafka.
