
#!/bin/sh


# {"at":"2019-12-09T08:13:26.86Z","env":"fromList [(\"value\",String \"Release-1.0.0\")]:0.1.10.1","ns":["cardano","node-metrics"],"data":{},"app":[],"msg":"Stat.utime = 1756","pid":"24239","loc":null,"host":"","sev":"Notice","thread":"38"}


sed -ne '/"Stat.[us]time/{ s/"at":/\n&/; s/"msg":/\n&/; p; }' $* | cut -c -300 - | sed -ne '
    s/^"at":"\([0-9-]\+\)T\([0-9:.]\+\)Z.*/\1 \2/ 
    t keep
    b cont
    : keep
    h; n
    : cont
    s/^"msg":"Stat.\([^ ]\+\) = \([0-9]\+\)".*/\1;\2/
    t good
    d
    : good
    G
    s/\n/;/g
    p
   '

