Goal: to collate various network (and file?) encodings, for future reference.

Idea: examine a single network protocol and, based on what makes it different or odd, catalogue the patterns.

For example:

Kafka:
- A binary protocol.
- TCP, stream-based.
- Big-endian byte ordering.
- There's a degree of asymmetry between requests and responses (different headers).
  - Response messages only have the correlation ID; you need to have seen the request message to know what kind of response it is.
- Messages can overlap (and out of order?). Correlation IDs are used to marry responses with requests.
  - Compare with other protocols, where it's strictly request-then-response.
    - Note that even request-response protocols can be pipelined (multiple requests in flight at once; if you squint hard, most protocols can be used like this, provided you're willing to ignore errors).
- Messages have a fixed schema, rather than using field tags (e.g.).
  - except for tagged fields, added in KIP-whatever.
  - It allows for variable length fields, rather than being entirely fixed-length.
    - TODO: find an example of a fixed-length protocol.
  - Earlier versions use fixed-length size prefixes for the variable-length bits.
  - Later versions use variable-length integer encodings (also for size prefixes).
- Versioning is per-message, where the message is identified by (key, version).
  - Compare with other protocols where there's an overall version implied.
  - Version negotiation (per-message, remember) is expected (but not necessarily?) done at connection time, by sending a specific negotiation message, rather than as part of the connection handshake.
    - compare with ???
- TLS: it's implicit in the listener config (a la HTTPS), rather than STARTLS-style.
- Auth: TODO

Compare with, say:
- Redis
- AMQP
- MQTT
- UPnP/SSDP
- MySQL, PostgreSQL, T-SQL (whatever it's called)
- RPC, all flavours (DCE-RPC/NDR, Sun RPC/XDR)

Also the basics:
- TCP, UDP, IP
- various serial protocols -- XModem, ZModem, etc. -- which opens up the opportunity to talk about CRC/checksums/etc.

Then there's the various encapsulation protocols, like PPPoE, other VPN stuff.

TLS/ASN.1 has got to have some juicy serialization choices. In fact: why don't people use ASN.1 for more things? Because it's insecure, that's why. Oh: interesting point: security implications of these choices, including buffer overflows, DoS, etc.

Not directly related to the encoding, but upper sizes on messages, timeouts, etc., they're all interesting parts of network protocols.

Text-based protocols:
- HTTP/1.1
- SMTP

Mixed protocols:
- HTTP sending binaries.
- SMTP sending binaries.
