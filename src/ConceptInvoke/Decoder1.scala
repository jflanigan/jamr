package edu.cmu.lti.nlp.amr.ConceptInvoke
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.Train._
import edu.cmu.lti.nlp.amr.BasicFeatureVector._

import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable => m, immutable => i}

/*** Defined in package.scala ***
type PhraseConceptPair = (List[String], String, PhraseConceptFeatures)
********************************/

class Decoder1(options: m.Map[Symbol, String],
               featureNames: List[String],
               phraseConceptPairs: Array[PhraseConceptPair],
               useNER: Boolean = true)
    extends Decoder(featureNames) {
    // Base class has defined:
    // val features: Features

    val conceptInvoker = new Concepts(options, phraseConceptPairs)

    def decode(input: Input, cost: (Input, PhraseConceptPair, Int, Int) => Double) : DecoderResult = {
        logger(1, "\n--- Decoder1 ---\n")
        logger(1, "Sentence: "+input.sentence.mkString(" "))
        //logger(1, "Weights:\n"+features.weights.toString)
        val sentence = input.sentence
        val bestState : Array[Option[(Double, PhraseConceptPair, Int)]] = sentence.map(x => None)    // (score, concept, backpointer)
        for (i <- Range(0, sentence.size)) {
            logger(2, "word = "+sentence(i))
            var conceptList = conceptInvoker.invoke(input,i)
            logger(2, "Possible invoked concepts: "+conceptList)
            // WARNING: the code below assumes that anything in the conceptList will not extend beyond the end of the sentence (and it shouldn't based on the code in Concepts)
            for (concept <- conceptList) {
                val score = (features.localScore(input, concept, i, i + concept.words.size)
                            + cost(input, concept, i, i + concept.words.size))
                logger(1, "concept = "+concept.toString)
                val endpoint = i + concept.words.size - 1
                logger(2, "score = "+score.toInt)
                if ((bestState(endpoint) == None && score >= 0) || (bestState(endpoint) != None && bestState(endpoint).get._1 <= score)) { // we use <= so that earlier concepts (i.e. ones our conceptTable) have higher priority
                    logger(2, "adding concept:"+concept)
                    bestState(endpoint) = Some((score, concept, i))
                }
            }
        }

        logger(2, "Chart = " + bestState.toList)

        // Follow backpointers
        var graph = Graph.Null
        var score = 0.0
        val feats = new FeatureVector()
        var i = bestState.size - 1
        graph.getNodeById.clear
        graph.getNodeByName.clear
        while (i >= 0) {
            if (bestState(i) != None) {
                val (localScore, concept, backpointer) = bestState(i).get
                logger(2, "Adding concept: "+concept.graphFrag)
                graph.addSpan(sentence, backpointer, i+1, concept.graphFrag)
                for (c <- conceptInvoker.invoke(input,i).filter(x => x.words == concept.words && x.graphFrag == concept.graphFrag)) { // add features for all matching phraseConceptPairs (this is what the Oracle decoder does, so we do the same here)
                    val f = features.localFeatures(input, c, i, i + concept.words.size)
                    feats += f
                    score += features.weights.dot(f) + cost(input, c, i, i + concept.words.size)
                    logger(1, "\nphraseConceptPair: "+concept.toString)
                    logger(1, "feats:\n"+f.toString)
                    logger(1, "score:\n"+score.toString)
                }
                //feats += features.localFeatures(input, concept)
                //score += localScore
                i = backpointer
            }
            i -= 1
        }
        if (graph.getNodeById.size == 0) {  // no invoked concepts
            graph = Graph.AMREmpty
        }
        logger(1, "Decoder1 Spans:")
        for ((span, i) <- graph.spans.sortBy(x => x.words.toLowerCase).zipWithIndex) {
            logger(1, "Span "+(i+1).toString+":  "+span.words+" => "+span.amr)
        }
        logger(1, "Decoder1 feats:\n"+feats.toString)
        return DecoderResult(graph, feats, score)
    }

}

