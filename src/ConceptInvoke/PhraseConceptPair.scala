package edu.cmu.lti.nlp.amr.ConceptInvoke
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.BasicFeatureVector._

import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable => m, immutable => i}

case class PhraseConceptPair(words: List[String], graphFrag: String, features: FeatureVector, trainingIndices: List[Int] = List()) {

/* The format of the phrase-concept table is
expert ||| (person :ARG1-of expert-41) ||| Count=4 ConceptGivenPhrase=0.3077 ||| 100 233 10001
   or (with no training indices specified)
expert ||| (person :ARG1-of expert-41) ||| Count=4 ConceptGivenPhrase=0.3077
*/

    override def toString : String = {     // TODO
        return words.mkString(" ")+" ||| "+graphFrag+" ||| "+features.fmap.toList.map(x => x._1+"="+x._2).sorted.mkString(" ")+" ||| "+trainingIndices.mkString(" ")
    }

    def graph : Graph = {   // Be careful, this is a potentially expensive operation
        return Graph.parse(graphFrag)
    }

}

object PhraseConceptPair {
    def apply(string: String) : PhraseConceptPair = {
        val fields = string.split(""" \|\|\| """)
        val words = fields(0).split(" ").toList
        val graphFrag = fields(1)
        val Feat = """(.+)=([^=]+)""".r
        val features = m.Map() ++ fields(2).split(" ").map(x => { val Feat(name, v) = x; (name, v.toDouble) }).toMap
        val trainingIndices = if (fields.size > 3) {
                fields(3).split(" ").toList.map(_.toInt)
            } else {
                List()
            }
        return new PhraseConceptPair(words, graphFrag, FeatureVector(features), trainingIndices)
    }

/*  This would be usefull for having an explit None concept (requires changes to the decoder, may not be compatible with joint decoder)
    def None(input: Input, start: Int, end: Int, nonBias: Boolean, nonLength: Boolean) = {
        val feats : m.Map[String, Double] = m.Map()
        if (nonLength) {
            feats("NONELen") = end - start
        }
        if (nonBias) {
            feats("NONEBias") = 1.0
        }
        return PhraseConceptPair(input.sentence.slice(start, end), "<NONE_CONCEPT>", FeatureVector(feats), List())
    } */
}

