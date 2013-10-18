package edu.cmu.lti.nlp.amr.GraphDecoder
import edu.cmu.lti.nlp.amr._

import java.io.File
import java.io.FileOutputStream
import java.io.PrintStream
import java.io.BufferedOutputStream
import java.io.OutputStreamWriter
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

class Alg1(featureNames: List[String], labelSet: Array[String])
    extends Decoder(featureNames, labelSet) {
    // Base class has defined:
    // val features: Features
    // var neighbors: (Node) => Iterator[Node]
    // var labels   (set of labels represented as an array)
    // var nodes

    def decode(input: Input) : DecoderResult = {
        // Assumes that Node.relations has been setup correctly for the graph fragments
        val Input(graph, sentence, parse) = input
        nodes = graph.nodes

        logger(1, "weights = " + features.weights)

        var score = 0.0
        val feats = new FeatureVector()
        for { node1 <- nodes
              relations = node1.relations.map(_._1).toSet
              label <- labels
              if !relations.contains(label) } {
            logger(1, "node1 = " + node1.concept)

            // Search over neighbors, and pick the one with highest score
            val (weight, node2) = neighbors(node1).map(x => (features.localScore(node1, x, label, input), x)).maxBy(_._1)

            logger(1, "node2 = " + node2.concept)
            logger(1, "weight = " + weight.toString)

            if (weight > 0) {
                // Add the relation to the graph
                node1.relations = (label, node2) :: node1.relations
                feats += features.localFeatures(node1, node2, label, input)
                score += weight
            }
        }

        graph.doRecursive(node => { node.relations = node.relations.reverse })
        return DecoderResult(graph, feats, score)
    }
}

