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
    val features = decoder.features
    decoder.features.addFeatureFunction("CostAugEdgeId")
    decoder.features.addFeatureFunction("rootCostAug")

    def decode(input: Input) : DecoderResult = {                        // WARNING: input should be same as input to oracle decoder
        val oracleDecoder = new Oracle(decoder.features.featureNames,   // "CostAugEdgeId" and "rootCostAug" already in featureNames
                                       decoder.features.weights.labelset)
        val oracle = oracleDecoder.decode(input)
        val addCost = oracle.features.filter(x => x.startsWith("CA:"))
        for ((feat, values) <- addCost.fmap) {
            values.unconjoined = 1.0
            values.conjoined = Map()
        }

        // add costScale to edge weights that don't match oracle (penalize precision type errors) (Actually add to all weights, will subtract)
        features.weights += (2.0 * precRecTradeoff * costScale) * addCost
        // subtract costScale from ones that match (penalize recall type errors) (Actually subtract twice the amount to cancel adding to all)
        features.weights -= (2.0 * (1.0 - 2.0 * precRecTradeoff) * costScale) * oracle.features.filter(x => x.startsWith("CA:"))

        // We want to do this:
        //val result = decoder.decode(Input(input.inputAnnotatedSentence, input.graph.duplicate.clearEdges))
        // Instead we do this:
        val saveGraph = input.graph.get
        input.graph = Some(saveGraph.duplicate.clearEdges)
        val result = decoder.decode(input)
        input.graph = Some(saveGraph)

        val score = features.weights.dot(result.features)

        // undo the changes
        features.weights -= (2.0 * precRecTradeoff * costScale) * addCost
        features.weights += (2.0 * (1.0-2.0 * precRecTradeoff) * costScale) * oracle.features.filter(x => x.startsWith("CA:"))

        val feats = result.features.filter(x => !x.startsWith("CA:"))
        return DecoderResult(result.graph, feats, score)
    }
}

