package edu.cmu.lti.nlp
import scala.language.implicitConversions

package object amr {
    implicit def doubleToMulAssoc(x: Double) = new MulAssoc(x)
    var verbosity = 1
    def logger(n: Int, s: Any) { if(n<=verbosity) System.err.println(s) }
}

