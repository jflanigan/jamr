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
import scala.collection.mutable.PriorityQueue
import Double.{NegativeInfinity => minusInfty}

class Alg2(featureNames: List[String], labelSet: Array[String])
    extends Decoder(featureNames) {
    // Base class has defined:
    // val features: Features
    // var neighbors: (Node) => Iterator[Node]
    // var nodes

/*    var neighbors : Array[Array[(Node, String, Double)]] = Array()

    def mkNeighbors(input: Input) : Array[Array[(Node, String)]] = {
        for (node1 <- nodes) yield {
            for (node2 <- nodes) yield {
                val (label, weight) = labelSet.map(x => (x, features.localScore(node1, node2, x, input))).maxBy(_._1)
                (node2, label, weight)
            }
        }
    } */

    def decode(input: Input) : DecoderResult = {
        // Assumes that Node.relations has been setup correctly for the graph fragments
        val Input(graph, sentence, parse) = input
        nodes = graph.nodes
//        neighbors = mkNeighbors(input)
        
        val set : Array[Int] = nodes.zipWithIndex.map(_._2)
        val setArray : Array[Set[Int]] = nodes.zipWithIndex.map(x => Set(x._2))
        def getSet(nodeIndex : Int) : Set[Int] = { setArray(set(nodeIndex)) }

        var score = 0.0
        var feats = new FeatureVector()
        def addRelation(node1: Node, index1: Int, node2: Node, index2: Int, label: String, weight: Double) {
            node1.relations = (label, node2) :: node1.relations
            feats += features.localFeatures(node1, node2, label, input)
            score += weight
            if (set(index1) != set(index2)) {
                getSet(index1) ++= getSet(index1)
                setArray(set(index2)) = Set()
                set(index2) = set(index1)
            }
        }

        val neighbors : Array[Array[(String, Double)]] = {
            for ((node1, index1) <- nodes.zipWithIndex) yield {
                for ((node2, index2) <- nodes.zipWithIndex) yield {
                val (label, weight) = labelSet.map(x => (x, features.localScore(node1, node2, x, input))).maxBy(_._1)
                    if (weight > 0) {   // Add all positive weights
                        addRelation(node1, index1, node2, index2, label, weight)
                    }
                    (label, weight)
                }
            }
        }

        // Add negative weights to the queue
        val queue = new PriorityQueue[(Double, Int, Int, String)]()(Ordering.by(x => -x._1))
        if (getSet(0).size != nodes.size) {
            for { (node1, index1) <- nodes.zipWithIndex
                  ((label, weight), index2) <- neighbors(index1).zipWithIndex
                  if weight <= 0 && set(index1) != set(index2) } {
                queue.enqueue((weight, index1, index2, label))
            }
        }

        // Kruskal's algorithm
        while (getSet(0).size != nodes.size) {
            val (weight, index1, index2, label) = queue.dequeue
            if (set(index1) != set(index2)) {
                addRelation(nodes(index1), index1, nodes(index2), index2, label, weight)
            }
        }

        graph.doRecursive(node => { node.relations = node.relations.reverse })
        return DecoderResult(graph, feats, score)
    }
}

