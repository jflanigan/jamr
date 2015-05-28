package edu.cmu.lti.nlp.amr.ConceptInvoke
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.Train._
import edu.cmu.lti.nlp.amr.BasicFeatureVector._

import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable => m, immutable => i}

abstract class Decoder(featureNames: List[String], phraseCounts: i.Map[List[String], Int]) {
    val features = new Features(featureNames, phraseCounts) // maybe this should be renamed ff?

    def decode(input: Input, trainingIndex: Option[Int], cost: (Input, PhraseConceptPair, Int, Int, List[PhraseConceptPair]) => Double = (i,c,s,p,l) => 0) : DecoderResult
}

