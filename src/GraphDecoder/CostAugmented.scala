package edu.cmu.lti.nlp.amr.GraphDecoder
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.FastFeatureVector._

import java.lang.Math.abs
import java.lang.Math.log
import java.lang.Math.exp
import java.lang.Math.random
import java.lang.Math.floor
import java.lang.Math.min
import java.lang.Math.max
import scala.io.Source
import scala.util.matching.Regex
//import scala.collection.mutable.Map
import scala.collection.concurrent.{TrieMap => Map}
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

class CostAugmented(val decoder: Decoder, costScale: Double, precRecTradeoff: Double) extends Decoder {
    // precRecTradeoff: 1 = only prec errors, 0 = only recall errors
    var features = decoder.features
    decoder.features.addFeatureFunction("CostAugEdge")
    decoder.features.addFeatureFunction("rootCostAug")

    def decode(input: Input) : DecoderResult = {                        // WARNING: input should be same as input to oracle decoder
        val oracleDecoder = new Oracle(decoder.features.featureNames,   // "CostAugEdgeId" and "rootCostAug" already in featureNames
                                       decoder.features.weights.labelset)
        val oracle = oracleDecoder.decode(input)
        val addCost = new FeatureVector(oracle.features.labelset)
        var edgeFeatures : List[(String, ValuesList)] = List()
        for { node1 <- input.graph.get.nodes
              node2 <- input.graph.get.nodes
            } {
                edgeFeatures = ("CA:U_C1="+node1.concept+"+C2="+node2.concept, ValuesList(1.0, List())) :: edgeFeatures
        }
        addCost += edgeFeatures     // Add features not conjoined with label (aka unconjoined)

        // add costScale to edge weights that don't match oracle (penalize precision type errors) (Actually add to all weights, then subtract)
        // (penalize predicting edge that isn't in oracle)
        features.weights += (1.0 * precRecTradeoff * costScale) * addCost
        features.weights -= (1.0 * precRecTradeoff * costScale) * oracle.features.filter(x => x.startsWith("CA:C1"))
        // subtract costScale from ones that match (penalize recall type errors) (Actually subtract twice the amount to cancel adding to all)
        // (penalize not predicting edge that is in oracle)
        features.weights -= (1.0 * (1.0 - precRecTradeoff) * costScale) * oracle.features.filter(x => x.startsWith("CA:C1"))

        // We want to do this:
        //val result = decoder.decode(Input(input.inputAnnotatedSentence, input.graph.duplicate.clearEdges))
        // Instead we do this:
        val saveGraph = input.graph.get
        input.graph = Some(saveGraph.duplicate.clearEdges)  // WARNING: this code should follow what AMRTrainingData.toInputGraph() does
        input.graph.get.normalizeInverseRelations           // need to call this because clearEdges resets the edges
        input.graph.get.addVariableToSpans                  // need to call this because clearEdges resets the variables
        val result = decoder.decode(input)
        input.graph = Some(saveGraph)

        val score = features.weights.dot(result.features)

        // undo the changes
        features.weights -= (1.0 * precRecTradeoff * costScale) * addCost
        features.weights += (1.0 * precRecTradeoff * costScale) * oracle.features.filter(x => x.startsWith("CA:"))
        features.weights += (1.0 * (1.0 - precRecTradeoff) * costScale) * oracle.features.filter(x => x.startsWith("CA:"))

        val feats = result.features.filter(x => !x.startsWith("CA:"))
        return DecoderResult(result.graph, feats, score)
    }
}

