package edu.cmu.lti.nlp.amr.GraphDecoder
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
import Double.{NegativeInfinity => minusInfty}

class CostAugmented(val decoder: Decoder, costScale: Double) extends Decoder {
    val features = decoder.features
    decoder.features.addFeatureFunction("CostAugEdgeId")
    decoder.features.addFeatureFunction("rootCostAug")

    def decode(input: Input) : DecoderResult = {
        val oracleDecoder = new Oracle(decoder.features.featureNames)   // "CostAugEdgeId" and "rootCostAug" already in featureNames
        val oracle = oracleDecoder.decode(input)
        features.weights -= costScale * oracle.features.slice(x => x.startsWith("CA:"))
        val saveGraph = input.graph.get
        input.graph = Some(saveGraph.duplicate.clearEdges)
        val result = decoder.decode(input)
        input.graph = Some(saveGraph)
        features.weights += costScale * oracle.features.slice(x => x.startsWith("CA:"))
        val feats = result.features.slice(x => !x.startsWith("CA:"))
        return DecoderResult(result.graph, feats, features.weights.dot(feats))
    }
}

