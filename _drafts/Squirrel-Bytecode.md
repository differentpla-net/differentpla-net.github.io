# Squirrel Bytecode

## Overview

Given the following squirrel:

    print("Hello World");

You end up with a file that looks like this:

    0000000: fa fa 52 49 51 53 01 00 00 00 04 00 00 00 04 00  ..RIQS..........
    0000010: 00 00 54 52 41 50 10 00 00 08 09 00 00 00 68 65  ..TRAP........he
    0000020: 6c 6c 6f 2e 6e 75 74 10 00 00 08 04 00 00 00 6d  llo.nut........m
    0000030: 61 69 6e 54 52 41 50 02 00 00 00 02 00 00 00 00  ainTRAP.........
    0000040: 00 00 00 02 00 00 00 02 00 00 00 00 00 00 00 04  ................
    0000050: 00 00 00 00 00 00 00 54 52 41 50 10 00 00 08 05  .......TRAP.....
    0000060: 00 00 00 70 72 69 6e 74 10 00 00 08 0b 00 00 00  ...print........
    0000070: 48 65 6c 6c 6f 20 57 6f 72 6c 64 54 52 41 50 10  Hello WorldTRAP.
    0000080: 00 00 08 04 00 00 00 74 68 69 73 10 00 00 08 05  .......this.....
    0000090: 00 00 00 76 61 72 67 76 54 52 41 50 54 52 41 50  ...vargvTRAPTRAP
    00000a0: 10 00 00 08 05 00 00 00 76 61 72 67 76 01 00 00  ........vargv...
    00000b0: 00 00 00 00 00 03 00 00 00 10 00 00 08 04 00 00  ................
    00000c0: 00 74 68 69 73 00 00 00 00 00 00 00 00 03 00 00  .this...........
    00000d0: 00 54 52 41 50 01 00 00 00 00 00 00 00 02 00 00  .TRAP...........
    00000e0: 00 03 00 00 00 54 52 41 50 54 52 41 50 00 00 00  .....TRAPTRAP...
    00000f0: 00 08 02 00 03 01 00 00 00 01 04 00 00 02 00 00  ................
    0000100: 00 06 ff 03 02 00 00 00 00 17 ff 00 00 54 52 41  .............TRA
    0000110: 50 05 00 00 00 00 01 00 00 00 4c 49 41 54        P.........LIAT

Let's take it apart:

## Header

File begins with `0xFAFA` (`SQ_BYTECODE_STREAM_TAG`):

    0000000: fa fa 

## Closure

Then there's a serialized closure -- the squirrel interpreter calls
`sq_readclosure`.

A closure is `SQ_CLOSURESTREAM_HEAD` ('SQIR'), followed by `sizeof(SQChar)`,
`sizeof(SQInteger)`, `sizeof(SQFloat)`; these are (presumably) used to check
that the bytecode was written on the same architecture as it's now being loaded
on.

    0000000:       52 49 51 53 01 00 00 00 04 00 00 00 04 00    RIQS..........
    0000010: 00 00                                            ..

So:

    head  = 0x53514952 ('SQIR')
    sizeC = 0x00000001
    sizeI = 0x00000004
    sizeF = 0x00000004

This is followed by an `SQFunctionProto` (see below), then
`SQ_CLOSURESTREAM_TAIL` ('TAIL'):

    0000110:                               4c 49 41 54                  LIAT

Note that 'SQIR' and 'TAIL' appear as 'RIQS' and 'LIAT' in the file
(little-endian).

## SQFunctionProto

It starts with 'PART':

    0000010:       54 52 41 50                                  TRAP

...followed by `sourcename`, and `name`, which are both squirrel objects.  A
squirrel object starts with a 32-bit type. Here we see 0x08000010, which is
`OT_STRING`. Strings are serialized with a length prefix, here 0x00000009,
followed by a string, here "hello.nut" (9 octets).

`sourcename`:

    0000010:                   10 00 00 08 09 00 00 00 68 65        ........he
    0000020: 6c 6c 6f 2e 6e 75 74                             llo.nut

`name`: `OT_STRING`, 4 octets, "main":

    0000020:                      10 00 00 08 04 00 00 00 6d         ........m
    0000030: 61 69 6e 54                                      ain

Aside: so it would appear that squirrel programs have an implicit `main`
function.

Then:

    head           = 'PART'
    nliterals      = 0x00000002
    nparameters    = 0x00000002
    noutervalues   = 0x00000000
    nlocalvarinfos = 0x00000002
    nlineinfos     = 0x00000002
    ndefaultparams = 0x00000000
    ninstructions  = 0x00000004
    nfunctions     = 0x00000000

    0000030:          54 52 41 50 02 00 00 00 02 00 00 00 00     TRAP.........
    0000040: 00 00 00 02 00 00 00 02 00 00 00 00 00 00 00 04  ................
    0000050: 00 00 00 00 00 00 00                             .......

### Literals

Then the literals:

    head       = 'PART'
    literal[0] = OT_STRING, 5 octets, "print".
    literal[1] = OT_STRING, 11 (0x0b) octets, "Hello World"

    0000050:                      54 52 41 50 10 00 00 08 05         TRAP.....
    0000060: 00 00 00 70 72 69 6e 74 10 00 00 08 0b 00 00 00  ...print........
    0000070: 48 65 6c 6c 6f 20 57 6f 72 6c 64                 Hello World

### Parameters

Then the parameters:

    head = 'PART'
    parameter[0] = OT_STRING, 4 octets, "this"
    parameter[1] = OT_STRING, 5 octets, "vargv".

    0000070:                                  54 52 41 50 10             TRAP.
    0000080: 00 00 08 04 00 00 00 74 68 69 73 10 00 00 08 05  .......this.....
    0000090: 00 00 00 76 61 72 67 76                          ...vargv        

### Outer Values

Then the outer values, which is empty, so it's just `'PART'`. Note: I've left
the following `'PART'` for clarity:

    0000090:                         54 52 41 50 54 52 41 50          TRAPTRAP

### Local Var Infos

Then the local var infos, which are `{obj, pos, start_op, end_op}`:

    head = 'PART'
    lvi[0] = type = OT_STRING, len = 5, name = "vargv", pos = 1, start_op = 0, end_op = 3
    lvi[1] = type = OT_STRING, len = 4, name = "this", pos = 0, start_op = 0, end_op = 3

    0000090:                                     54 52 41 50              TRAP
    00000a0: 10 00 00 08 05 00 00 00 76 61 72 67 76 01 00 00  ........vargv...
    00000b0: 00 00 00 00 00 03 00 00 00 10 00 00 08 04 00 00  ................
    00000c0: 00 74 68 69 73 00 00 00 00 00 00 00 00 03 00 00  .this...........
    00000d0: 00                                               .

### Line Infos

Then the line infos:

    head = 'PART'
    li[0] = { line = 1, op = 0 }
    li[1] = { line = 2, op = 3 }

    00000d0:    54 52 41 50 01 00 00 00 00 00 00 00 02 00 00   TRAP...........
    00000e0: 00 03 00 00 00                                   .....           

### Default params

Then no default params:
    
    00000e0:                54 52 41 50 54 52 41 50 00 00 00       TRAPTRAP...

### Instructions

Then the instructions, which are `SQInstruction` objects:

    00000e0:                            54 52 41 50 00 00 00           TRAP...
    00000f0: 00 08 02 00 03 01 00 00 00 01 04 00 00 02 00 00  ................
    0000100: 00 06 ff 03 02 00 00 00 00 17 ff 00 00 54 52 41  .............TRA

An instruction is:

    union { int arg1; float arg1 };
    char op;
    char arg0;
    char arg2;
    char arg3;

I'm assuming it's this way to get tighter packing.

So these are:

#### 0

    00000e0:                                        00 00 00
    00000f0: 00 08 02 00 03

`0x08(0x02, 0x00000000, 0x00, 0x03)`
`0x08` is `_OP_PREPCALLK`

    o = STK(arg2 = 0x00);
    Get(o, GetLiteral(arg1 == 0x00000000));
    STK(arg3 = 0x03) = o;

I _think_ that this is looking for literal 0 on the root table; i.e. global
`print`. It's then storing it at stack #3.

    PREPCALLK       ; 08
        STK(0)      ; 00
        STK(3)      ; 03
        LITERAL(0)  ; 00 00 00 00

So: what's the 0x02 doing in there?

#### 1

    00000f0:                01 00 00 00 01 04 00 00 

`0x01(0x04, 0x00000001, 0x00, 0x00)`
`0x01` is `_OP_LOAD`:

    TARGET = GetLiteral(arg1 = 0x00000001); continue;

This stores literal 1 ("Hello World") at stack #arg0 (4). That is: `TARGET`
always refers to STK(arg0).

    LOAD            ; 01
        STK(4)      ; 04
        LITERAL(1)  ; 01 00 00 00
        ;; unused 00 00

#### 2

    00000f0:                                        02 00 00  ................
    0000100: 00 06 ff 03 02

`0x06(0xff, 0x00000002, 0x03, 0x02)`
`0x06` is `_OP_CALL`

    clo = STK(arg1 = 0x00000002);
    switch(type(clo))

_???_

#### 3

    0000100:                00 00 00 00 17 ff 00 00

`0x17(0xff, 0x00000000, 0x00, 0x00)`
`0x17` is `_OP_RETURN`

_???_

This is a very weird stack-based language.

### Functions (none)

    head = 'PART'
    
    0000100:                                        54 52 41  .............TRA
    
### Extras

    stacksize  = 0x00000005
    bgenerator = 0x00 (false)
    varparams  = 0x00000001

    0000100:                                        54 52 41  .............TRA
    0000110: 50 05 00 00 00 00 01 00 00 00                    P.........

And that's it.

Next time: how to actually run this code.
