package edu.cmu.lti.nlp.amr.ConceptInvoke
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

case class PhraseFeatures(count: Double, conceptGivenPhrase: Double) {

    def this(string: String) = this(
        string.split(" ")(0),
        string.split(" ")(1))

}

