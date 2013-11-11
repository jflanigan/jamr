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

class Alg1(featureNames: List[String], labelSet: Array[(String, Int)], rootedConstraint: Boolean = true)
    extends Decoder(featureNames) {
    // Base class has defined:
    // val features: Features

    def decode(input: Input) : DecoderResult = {
        // Assumes that Node.relations has been setup correctly for the graph fragments
        val graph = input.graph.duplicate
        val nodes : List[Node] = graph.nodes.toList
        features.input = input

        def neighbors(node: Node) : List[Node] = {
            nodes
        }

        logger(1, "Alg1")
        //logger(2, "weights = " + features.weights)

        var score = 0.0
        val feats = new FeatureVector()
        for { node1 <- nodes
              relations = node1.relations.map(_._1).toSet
              (label, maxCardinality) <- labelSet } {

            if (relations.contains(label)) {
                for ((_, node2) <- node1.relations.filter(_._1 == label)) {
                    feats += features.localFeatures(node1, node2, label)
                    score += features.localScore(node1, node2, label)
                }
            } else {
                // Search over neighbors, and pick the ones with highest score
                val nodes2 : List[(Node, Double)] = neighbors(node1).map(x => (x, features.localScore(node1, x, label))).filter(x => x._2 > 0 && x._1.id != node1.id).sortBy(-_._2).take(maxCardinality)

                for ((node2, weight) <- nodes2) {
                    node1.relations = (label, node2) :: node1.relations
                    feats += features.localFeatures(node1, node2, label)
                    score += weight
                    logger(0, "Adding edge ("+node1.concept+", "+label +", "+node2.concept + ") with weight "+weight.toString)
                }
                if (nodes2.size > 0) {
                    logger(1, "node1 = " + node1.concept)
                    logger(1, "label = " + label)
                    logger(1, "nodes2 = " + nodes.toString)
                    //logger(1, "feats = " + feats.toString)
                }
            }
        }

        if (features.rootFeatureFunctions.size != 0) {
            graph.root = nodes.map(x => (x, features.rootScore(x))).maxBy(_._2)._1
        } else {
            graph.root = nodes(0)
        }
        feats += features.rootFeatures(graph.root)

        nodes.map(node => { node.relations = node.relations.reverse })
        //graph.makeTopologicalOrdering()   // won't work - may not be connected
        return DecoderResult(graph, feats, score)
    }
}

