#!/bin/sh

sed -ne '/TraceAddBlockEvent.SwitchedToChain/{ s/"at":/\n&/; s/."newtip.":/\n&/; p; }' $* | cut -c -300 - | sed -ne '
    s/^"at":"\([0-9-]\+\)T\([0-9:.]\+\)Z.*/\1 \2/ 
    t keep
    b cont
    : keep
    h; n
    : cont
    s/^."newtip.":."\([0-9a-z]\+\).*/\1/
    t good
    d
    : good
    G
    s/\n/;/g
    p
   '

