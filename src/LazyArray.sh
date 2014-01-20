#!/bin/sh
exec scala -classpath . "$0" "$@"
!#

import edu.cmu.lti.nlp.amr.LazyArray

val i =Iterator("a", "b", "c")
val la = new edu.cmu.lti.nlp.amr.LazyArray(i)
println(la(1))
la.map(x => println("a"+x))(2)
Iterator("a", "b", "c").map(println(_))
for (j <- la) {
    println(j)
}

