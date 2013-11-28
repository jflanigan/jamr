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

class Decoder1(featureNames: List[String],
               phraseConceptPairs: Array[PhraseConceptPair],
               useNER: Boolean = true)
    extends Decoder(featureNames) {
    // Base class has defined:
    // val features: Features

    val conceptTable: Map[String, List[PhraseConceptPair]] = Map()  // maps the first word in the phrase to a list of phraseConceptPairs
    for ((phrase, graphFragment, feats) <- phraseConceptPairs) {
        val word = phrase(0)
        conceptTable(word) = (phrase, graphFragment, feats) :: conceptTable.getOrElse(word, List())
    }

    def decode(input: Input) : DecoderResult = {
        val sentence = input.sentence
        val bestState : Array[Option[(Double, PhraseConceptPair, Int)]] = sentence.map(x => None)    // (score, concept, backpointer)
        for (i <- Range(0, sentence.size)) {
            var conceptList = conceptTable(sentence(i)).filter(x => x._1 == sentence.slice(i, i+x._1.size).toList)
            if (useNER) {
                conceptList = input.ner.annotation.filter(_.start == i).map(x => PhraseConceptPair.Entity(x)) ::: conceptList
            }
            // WARNING: the code below assumes that anything in the conceptList will not extend beyond the end of the sentence (and it shouldn't based on the code above)
            for (concept <- conceptList) {
                val score = features.localScore(input, concept)
                val endpoint = i + concept.words.size - 1
                if (score >= 0 || bestState(endpoint) == None || bestState(endpoint).get._1 <= score) { // we use <= so that earlier concepts (i.e. ones our conceptTable) have higher priority
                    bestState(endpoint) = Some((score, concept, i))
                }
            }
        }

        // Follow backpointers
        val graph = Graph.empty
        var score = 0.0
        val feats = new FeatureVector()
        var i = bestState.size - 1
        while (i >= 0) {
            if (bestState(i) != None) {
                val (localScore, concept, backpointer) = bestState(i)
                graph.addSpan(backpointer, i+1, concept._2)
                feats += features.localFeatures(input, concept)
                score += localScore
                i = backpointer
            }
            i -= 1
        }
        return DecoderResult(graph, feats, score)
    }

}

