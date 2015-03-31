package edu.cmu.lti.nlp.amr.ConceptInvoke
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.Train._
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
import scala.collection.mutable.PriorityQueue
import Double.{NegativeInfinity => minusInfty}

/*** Defined in package.scala ***
type PhraseConceptPair = (List[String], String, PhraseConceptFeatures)
********************************/

class Oracle(options: Map[Symbol, String],
             featureNames: List[String],
             phraseConceptPairs: Array[PhraseConceptPair])
    extends Decoder(featureNames) {
    // Base class has defined:
    // val features: Features

    val conceptInvoker = new Concepts(options, phraseConceptPairs)

    def decode(input: Input,
               trainingIndex: Option[Int],
               cost: (Input, PhraseConceptPair, Int, Int) => Double) : DecoderResult = {

        assert(input.graph != None, "Error: stage1 oracle decoder was not given a graph")
        val graph = input.graph.get
        val sentence = input.sentence
        var score = 0.0
        val feats = new FeatureVector()

        logger(1, "\n--- Oracle Decoder ---\n")
        for ((span, i) <- graph.spans.sortBy(x => x.words.toLowerCase).zipWithIndex) {
            logger(0, "Oracle Span "+(i+1).toString+":  "+span.words+" => "+span.amr)
        }

        for (span <- graph.spans) {
            val words = span.words.split(" ").toList
            val conceptList = conceptInvoker.invoke(input, span.start, None /*trainingIndex*/)
            //logger(1, "words = "+words.toString)
            //logger(1, "conceptList = "+conceptList.toString)
            val matching = conceptList.filter(x => x.words == words && x.graphFrag == span.amr.prettyString(detail = 0, pretty = false, vars = Set.empty[String]))
            if (matching.size > 1) {
                logger(0, "WARNING: There is more than one matching concept fragment.  This should not occur.  Please check that Concepts.invoke does not return duplicates")
            }
            for (concept <- matching) {
                val f = features.localFeatures(input, concept, span.start, span.end)
                feats += f
                score += features.weights.dot(f) + cost(input, concept, span.start, span.end)
                logger(1, "\nphraseConceptPair: "+concept.toString)
                logger(1, "feats:\n"+f.toString)
                logger(1, "score:\n"+score.toString)
            }
        }

        logger(1, "Oracle feats:\n"+feats.toString)

        return DecoderResult(graph, feats, score)
    }

}

