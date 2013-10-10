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

abstract class Alg1(featureNames: List[String], label_set: Array[String])
    extends Decoder(featureNames, label_set) {
    // Base class has defined:
    // val features: Features
    // var weight: (Node, Node, String, Input) => Double
    // var labels
    // var nodes

    def decode(input: Input) : DecoderResult = {
        // Assumes that Node.relations has be setup correctly (for all graph fragments)
        val Input(graph, sentence, parse) = input
        nodes = graph.nodes

        for{node1 <- nodes
            relations = node1.relations.map(_._1).toSet
            label <- labels
            if !relations.contains(label)} {

            // Search over neighbors, and pick the one with highest score
            val (weight, node2) = neighbors(node1).map(x => (local_score(node1, x, label, input), x)).maxBy(_._1)

            if (weight > 0) {
                // Adds the relation to the graph
                node1.relations = (label, node2) :: node1.relations
            }
        }

        return DecoderResult(graph, new FeatureVector(), 0)
    }
}

