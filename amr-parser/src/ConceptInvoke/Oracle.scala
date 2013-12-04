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
import scala.collection.mutable.PriorityQueue
import Double.{NegativeInfinity => minusInfty}

/*** Defined in package.scala ***
type PhraseConceptPair = (List[String], String, PhraseConceptFeatures)
********************************/

class Oracle(featureNames: List[String],
             phraseConceptPairs: Array[PhraseConceptPair],
             useNER: Boolean = true)
    extends Decoder(featureNames) {
    // Base class has defined:
    // val features: Features

    val conceptInvoker = new Concepts(phraseConceptPairs)

    def decode(input: Input) : DecoderResult = {
        assert(input.graph != None, "Error: stage1 oracle decoder was not given a graph")
        val graph = input.graph.get
        val sentence = input.sentence
        var score = 0.0
        val feats = new FeatureVector()

        for (span <- graph.spans) {
            val words = span.words.split(" ")
            val conceptList = conceptInvoker.invoke(input, span.start)
            val matching = conceptList.filter(x => x.words == words)
            for (concept <- matching) {
                val f = features.localFeatures(input, concept)
                feats += f
                score += features.weights.dot(f)
            }
        }

        return DecoderResult(graph, feats, score)
    }

}

