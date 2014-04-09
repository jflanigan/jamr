package edu.cmu.lti.nlp.amr
import scala.language.implicitConversions

package object FastFeatureVector {
    implicit def doubleToFastMulAssoc(x: Double) = new FastMulAssoc(x)
    implicit def doubleToFastMul2Assoc(x: Double) = new FastMul2Assoc(x)
}

