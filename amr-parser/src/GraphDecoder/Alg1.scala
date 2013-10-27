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

class Alg1(featureNames: List[String], labelSet: Array[(String, Int)])
    extends Decoder(featureNames) {
    // Base class has defined:
    // val features: Features

    def decode(input: Input) : DecoderResult = {
        // Assumes that Node.relations has been setup correctly for the graph fragments
        val graph = input.graph.duplicate
        val nodes : List[Node] = graph.nodes.toList

        def neighbors(node: Node) : List[Node] = {
            nodes
        }

        logger(2, "Alg1")
        //logger(2, "weights = " + features.weights)

        var score = 0.0
        val feats = new FeatureVector()
        for { node1 <- nodes
              relations = node1.relations.map(_._1).toSet
              (label, maxCardinality) <- labelSet } {
            logger(2, "node1 = " + node1.concept)
            logger(2, "label = " + label)

            if (relations.contains(label)) {
                val Some((_, node2)) = node1.relations.find(_._1 == label)
                feats += features.localFeatures(node1, node2, label, input)
                score += features.localScore(node1, node2, label, input)
            } else {
                // Search over neighbors, and pick the ones with highest score
                val nodes2 : List[(Node, Double)] = neighbors(node1).map(x => (x, features.localScore(node1, x, label, input))).sortBy(-_._2).filter(_._2 > 0).take(maxCardinality)

                logger(2, "nodes2 = " + nodes.toString)

                for ((node2, weight) <- nodes2) {
                    node1.relations = (label, node2) :: node1.relations
                    feats += features.localFeatures(node1, node2, label, input)
                    score += weight
                }
            }
        }

//        graph.root = nodes.map(x => (x, features.rootScore(x, input))).maxBy(_._2)
//        features += features.rootFeatures(graph.root, input)

        nodes.map(node => { node.relations = node.relations.reverse })
//        graph.makeTopologicalOrdering()
        return DecoderResult(graph, feats, score)
    }
}

