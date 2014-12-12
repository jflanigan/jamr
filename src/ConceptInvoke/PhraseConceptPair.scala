package edu.cmu.lti.nlp.amr.ConceptInvoke
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.BasicFeatureVector._

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

case class PhraseConceptPair(words: List[String], graphFrag: String, features: FeatureVector) {

/* The format of the phrase-concept table is
expert ||| (person :ARG1-of expert-41) ||| Count=4 ConceptGivenPhrase=0.3077
*/

    //def toString : String = {     // TODO
    //}

}

object PhraseConceptPair {
    def PhraseConceptPair(string: String) : PhraseConceptPair = {
        val words = string.split(""" \|\|\| """)(0).split(" ").toList
        val graphFrag = string.split(""" \|\|\| """)(1)
        val Feat = """(.*)=([^=]*)""".r
        val features = Map() ++ string.split(""" \|\|\| """)(2).split(" ").map(x => { val Feat(name, v) = x; (name, v.toDouble) }).toMap
        return PhraseConceptPair(words, graphFrag, FeatureVector(features))
    }
}

