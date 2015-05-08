TODO
====

 - Change all instances of string.split to string.splitStr.  Update style guide to recomend use.

#Known bugs

 - `dev` Bug in MSCG? See Alg2 decoder [here](https://github.com/jflanigan/jamr-internal/blob/10360b0ca055087605375e3c450471c1d273780f/src/GraphDecoder/Alg2.scala#L116).  Basically, doesn't merge the sets into the lower set like it should.
 - `dev` Need to patch dev file (duplicate variable names) so Smatch can evaluate on LDC2013E117 dev.
 - `RelationID` Bug with :op inside strings

#Generator

 - Greedy aligments for data
 - Baseline for generating tree
 - Make sentence grammars faster (parallel, filter amr frags, etc)
 - Do error analysis
 - Morph analyse gigaword (and pos tag it), and use it to generate possible concept realizations.
 - Add state (pos features?)
 - Lowercase? remove the, a?
 - Gigaword LM
 - N-best synthetic rules

