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
    for (pair <- phraseConceptPairs) {
        val word = pair.words(0)
        conceptTable(word) = pair :: conceptTable.getOrElse(word, List())
        //logger(2, "conceptTable("+word+") = "+conceptTable(word))
    }

    def decode(input: Input) : DecoderResult = {
        val sentence = input.sentence
        val bestState : Array[Option[(Double, PhraseConceptPair, Int)]] = sentence.map(x => None)    // (score, concept, backpointer)
        for (i <- Range(0, sentence.size)) {
            logger(1, "word = "+sentence(i))
            var conceptList = conceptTable.getOrElse(sentence(i), List()).filter(x => x.words == sentence.slice(i, i+x.words.size).toList)
            if (useNER) {
                conceptList = input.ner.annotation.filter(_.start == i).map(x => PhraseConceptPair.entity(input, x)).toList ::: conceptList
            }
            logger(1, "Possible invoked concepts: "+conceptList)
            // WARNING: the code below assumes that anything in the conceptList will not extend beyond the end of the sentence (and it shouldn't based on the code above)
            for (concept <- conceptList) {
                val score = features.localScore(input, concept)
                val endpoint = i + concept.words.size - 1
                logger(1, "score = "+score.toInt)
                if ((bestState(endpoint) == None && score >= 0) || bestState(endpoint).get._1 <= score) { // we use <= so that earlier concepts (i.e. ones our conceptTable) have higher priority
                    logger(1, "adding concept:"+concept)
                    bestState(endpoint) = Some((score, concept, i))
                }
            }
        }

        logger(1, "Chart = " + bestState.toList)

        // Follow backpointers
        var graph = Graph.empty
        var score = 0.0
        val feats = new FeatureVector()
        var i = bestState.size - 1
        graph.getNodeById.clear
        graph.getNodeByName.clear
        while (i >= 0) {
            if (bestState(i) != None) {
                val (localScore, concept, backpointer) = bestState(i).get
                logger(1, "Adding concept: "+concept.graphFrag)
                graph.addSpan(sentence, backpointer, i+1, concept.graphFrag)
                feats += features.localFeatures(input, concept)
                score += localScore
                i = backpointer
            }
            i -= 1
        }
        if (graph.getNodeById.size == 0) {  // no invoked concepts
            graph = Graph.empty
        }
        return DecoderResult(graph, feats, score)
    }

}

