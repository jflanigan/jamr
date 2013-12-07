package edu.cmu.lti.nlp.amr.ConceptInvoke
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

case class PhraseConceptFeatures(count: Double,
                                 conceptGivenPhrase: Double,
                                 fromNER: Boolean) {

    def this(string: String) = this(
        string.split(" ").find(x => x.matches("N=.*")).getOrElse("=0.0").split("=")(1).toDouble,
        string.split(" ").find(x => x.matches("""c\|p=.*""")).getOrElse("=0.0").split("=")(1).toDouble,
        false)

}

