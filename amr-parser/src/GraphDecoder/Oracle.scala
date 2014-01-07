package edu.cmu.lti.nlp.amr.GraphDecoder
import edu.cmu.lti.nlp.amr._

import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

class Oracle(featureNames: List[String])
    extends Decoder(featureNames) {
    // Base class has defined:
    // val features: Features

    private var inputSave: Input = _
    def input : Input = inputSave
    def input_= (i: Input) {
        inputSave = i
        features.input = i
    }

    def decode() : DecoderResult = {
        val graph = input.graph.get
        var feats = new FeatureVector()

        for { node1 <- graph.nodes
              (label, node2) <- node1.relations } {
            feats += features.localFeatures(node1, node2, label)
        }
        feats += features.rootFeatures(graph.root)

        return DecoderResult(graph, feats, features.weights.dot(feats))
    }
}

