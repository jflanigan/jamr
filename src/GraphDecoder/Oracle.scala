package edu.cmu.lti.nlp.amr.GraphDecoder
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.FastFeatureVector._

import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

class Oracle(options: Map[Symbol, String], featureNames: List[String], labelSet: Array[String]) extends Decoder {
    var features = new Features(options, featureNames, labelSet)

    def decode(input: Input) : DecoderResult = {
        features.input = input
        val graph = input.graph.get
        var feats = new FeatureVector(labelSet)

        for { node1 <- graph.nodes
              (label, node2) <- node1.relations } {
            feats += features.localFeatures(node1, node2, label)
        }
        feats += features.rootFeatures(graph.root)

        return DecoderResult(graph, feats, features.weights.dot(feats))
    }
}

