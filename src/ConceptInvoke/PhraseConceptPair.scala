package edu.cmu.lti.nlp.amr.ConceptInvoke
import edu.cmu.lti.nlp.amr._

import java.lang.Math.abs
import java.lang.Math.log
import java.lang.Math.exp
import java.lang.Math.random
import java.lang.Math.floor
import java.lang.Math.min
import java.lang.Math.max
import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

case class PhraseConceptPair(words: List[String], graphFrag: String, features: PhraseConceptFeatures) {

/* The format of the phrase-concept table is
expert ||| (person :ARG1-of expert-41) ||| Count=4 ConceptGivenPhrase=0.3077
*/

    def this(string: String) = this(
        string.split(""" \|\|\| """)(0).split(" ").toList,
        string.split(""" \|\|\| """)(1),
        new PhraseConceptFeatures(string.split(""" \|\|\| """)(2))
    )

}

