TODO
====

 - Change all instances of string.split to string.splitStr.  Update style guide to recomend use.

#Known bugs

 - `dev` Bug in MSCG? See Alg2 decoder [here](https://github.com/jflanigan/jamr-internal/blob/10360b0ca055087605375e3c450471c1d273780f/src/GraphDecoder/Alg2.scala#L116).  Basically, doesn't merge the sets into the lower set like it should.

 - `dev` Need to patch dev file (duplicate variable names) so Smatch can evaluate on LDC2013E117 dev.

 - `RelationID` Bug with :op inside strings


