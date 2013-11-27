package edu.cmu.lti.nlp.amr.ConceptInvoke
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

case class PhraseFeatures(count: Double, conceptGivenPhrase: Double) {

    def this(string: String) = this(
        string.split(" ").find(x => x.matches("Count=.*")).getOrElse("=0.0").split("=")(1).toDouble,
        string.split(" ").find(x => x.matches("ConceptGivenPhrase=.*")).getOrElse("=0.0").split("=")(1).toDouble)

}

