package edu.cmu.lti.nlp.amr
import scala.language.implicitConversions

package object BasicFeatureVector {
    implicit def doubleToMulAssoc(x: Double) = new MulAssoc(x)
}

