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
               phraseCounts: i.Map[List[String],Int])
    extends Decoder(featureNames, phraseCounts) {
    // Base class has defined:
    // val features: Features

    val conceptInvoker = new Concepts(options, phraseConceptPairs)

    def decode(input: Input,
               trainingIndex: Option[Int],      // if we are training, index into the training data so we can do leave-one-out decoding
               cost: (Input, PhraseConceptPair, Int, Int, List[PhraseConceptPair]) => Double) : DecoderResult = {

        logger(1, "\n--- Decoder1 ---\n")
        logger(1, "Sentence: "+input.sentence.mkString(" "))
        //logger(1, "Weights:\n"+features.weights.toString)
        val sentence = input.sentence
        val bestState : Array[Option[(Double, PhraseConceptPair, Int)]] = sentence.map(x => None)    // (score, concept, backpointer)
        for (i <- Range(0, sentence.size)) {
            logger(2, "word = "+sentence(i))
            var conceptList = conceptInvoker.invoke(input, i, trainingIndex)
            //logger(1, "Possible invoked concepts: "+conceptList.map(x => x.toString).mkString("\n"))
            // WARNING: the code below assumes that anything in the conceptList will not extend beyond the end of the sentence (and it shouldn't based on the code in Concepts)
            for (concept <- conceptList) {
                if (concept.words.size + i > sentence.size) {
                    logger(0, "WARNING: concept fragment " + concept.graphFrag + " extends beyond the end of the sentence - I will ignore it.")
                } else {
                    val score = (features.localScore(input, concept, i, i + concept.words.size)
                                + cost(input, concept, i, i + concept.words.size, conceptList))
                    //logger(1, "concept = "+concept.graphFrag)
                    val endpoint = i + concept.words.size - 1
                    //logger(2, "score = "+score.toInt)
                    if ((bestState(endpoint) == None && score >= 0) || (bestState(endpoint) != None && bestState(endpoint).get._1 <= score)) { // we use <= so that earlier concepts (i.e. ones our conceptTable) have higher priority
                        bestState(endpoint) = Some((score, concept, i))
                    }
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
                //logger(1, "Adding concept: "+concept.graphFrag)
                graph.addSpan(sentence, start = backpointer, end = i+1, amrStr = concept.graphFrag)
                //logger(1, "words = "+concept.words.mkString(" "))
                val conceptList = conceptInvoker.invoke(input, backpointer, trainingIndex)
                for (c <- conceptList.filter(x => x.words == concept.words && x.graphFrag == concept.graphFrag)) { // add features for all matching phraseConceptPairs (this is what the Oracle decoder does, so we do the same here)
                    val f = features.localFeatures(input, c, backpointer, backpointer + concept.words.size)
                    feats += f
                    score += features.weights.dot(f) + cost(input, c, backpointer, backpointer + concept.words.size, conceptList)
                    //logger(2, "\nphraseConceptPair: "+concept.toString)
                    //logger(1, "feats:\n"+f.toString)
                    //logger(1, "score:\n"+score.toString+"\n")
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

