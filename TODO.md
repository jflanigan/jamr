TODO
====

 - Change all instances of `string.split` to `string.splitStr`.  Update style guide to recomend use.
 - Add `scalacOptions += "-optimize"` to `build.sbt`.  In general, make sure we have optimizations turned on (especially the one for `for` loops), and benchmark `for` loops to make sure they are fast.  Could also try out [the new optimizer](http://magarciaepfl.github.io/scala/)

#Known bugs

 - `dev` Bug in MSCG? See Alg2 decoder [here](https://github.com/jflanigan/jamr-internal/blob/10360b0ca055087605375e3c450471c1d273780f/src/GraphDecoder/Alg2.scala#L116).  Basically, doesn't merge the sets into the lower set like it should.
 - `dev` Need to patch dev file (duplicate variable names) so Smatch can evaluate on LDC2013E117 dev.
 - `RelationID` Bug with :op inside strings

#`RelationID`

 - stage2 bias feature scale, and use regularization when using infinite ramp

 - stage2 LR stepsize option

 - stage2 LR stepsize strategy

 - print out LR objective during LR decoding

 - print out size of smallest abs(weight) edge, as well as avg, max, and median excluding costs

 - adjust stepsize based on size of weights? (option to set stepsize to a multiple of the smallest abs(weight) edge in the dense graph, but not smaller than stepsize and give warning if it would be smaller)

 - stage2 LR stepsizes (try different ones in parallel)

 - L2 constrained for SSGD (maybe adjust stepsize so it's like unconstrained?) so that LR doesn't get harder as weights are updated


