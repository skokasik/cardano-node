#!/bin/sh


# byronBlockSlotNo = SlotNo {unSlotNo = 968}, byronBlockHash = ByronHash {unByronHash = AbstractHash 0abf6c78f4a80f6dd2dd05d77633ab      7690d8ad29debc36dc4bf820894197069e}}


sed -ne '/TraceForgeEvent/{ s/"at":/\n&/; s/byronBlockSlotNo = SlotNo/\n&/; p; }' $* | cut -c -300 - | sed -ne '
    s/^"at":"\([0-9-]\+\)T\([0-9:.]\+\)Z.*/\1 \2/ 
    t keep
    b cont
    : keep
    h; n
    : cont
    s/^byronBlockSlotNo = SlotNo {unSlotNo = \([0-9]\+\)}, byronBlockHash = ByronHash {unByronHash = AbstractHash \([a-z0-9]\{16\}\).*/\1;\2/
    t good
    d
    : good
    G
    s/\n/;/g
    p
   '

