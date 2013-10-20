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

class Alg2(featureNames: List[String], labelSet: Array[(String, Int)])
    extends Decoder(featureNames) {
    // Base class has defined:
    // val features: Features

    def decode(input: Input) : DecoderResult = {
        // Assumes that Node.relations has been setup correctly for the graph fragments
        val graph = input.graph.duplicate
        val nodes : Array[Node] = graph.nodes.toArray
        val nonDistinctLabels = labelSet.toList.filter(x => x._2 > 1)
        val distinctLabels = labelSet.filter(x => x._2 == 1)

        // Each node is numbered by its index in 'nodes'
        // Each set is numbered by its index in 'setArray'
        // 'set' contains the index of the set that each node is assigned to
        // At the start each node is in its own set
        val set : Array[Int] = nodes.zipWithIndex.map(_._2)
        val setArray : Array[Set[Int]] = nodes.zipWithIndex.map(x => Set(x._2))
        def getSet(nodeIndex : Int) : Set[Int] = { setArray(set(nodeIndex)) }

        var score = 0.0
        var feats = new FeatureVector()
        def addEdge(node1: Node, index1: Int, node2: Node, index2: Int, label: String, weight: Double) {
            node1.relations = (label, node2) :: node1.relations
            feats += features.localFeatures(node1, node2, label, input)
            score += weight
            if (set(index1) != set(index2)) {   // If different sets, then merge them
                logger(1, "Adding an edge")
                logger(1, "set = " + set.toList)
                logger(1, "setArray = " + setArray.toList)
                getSet(index1) ++= getSet(index2)
                getSet(index2).clear()
                set(index2) = set(index1)
            }
        }

        val neighbors : Array[Array[(String, Double)]] = {
            for ((node1, index1) <- nodes.zipWithIndex) yield {
                for ((node2, index2) <- nodes.zipWithIndex) yield {
                    val (label, weight) = distinctLabels.map(x => (x._1, features.localScore(node1, node2, x._1, input))).maxBy(_._2)
                    val ndLabels = nonDistinctLabels.map(x => (x._1, features.localScore(node1, node2, x._1, input))).filter(x => x._2 > 0 && x._1 != label)
                    ndLabels.map(x => addEdge(node1, index1, node2, index2, x._1, x._2))
                    if (weight > 0) {   // Add all positive weights
                        addEdge(node1, index1, node2, index2, label, weight)
                    }
                    val (label2, weight2) = ndLabels.maxBy(_._2)
                    if (weight > weight2) {
                        (label, weight)
                    } else {
                        (label2, weight2)
                    }
                }
            }
        }

        // Add negative weights to the queue
        val queue = new PriorityQueue[(Double, Int, Int, String)]()(Ordering.by(x => x._1))
        if (getSet(0).size != nodes.size) {
            for { (node1, index1) <- nodes.zipWithIndex
                  ((label, weight), index2) <- neighbors(index1).zipWithIndex
                  if weight <= 0 && set(index1) != set(index2) } {
                queue.enqueue((weight, index1, index2, label))
            }
        }

        // Kruskal's algorithm
        logger(1, queue.toString)
        logger(1, set.toList)
        logger(1, setArray.toList)
        while (getSet(0).size != nodes.size) {
            logger(1, queue.toString)
            val (weight, index1, index2, label) = queue.dequeue
            if (set(index1) != set(index2)) {
                addEdge(nodes(index1), index1, nodes(index2), index2, label, weight)
            }
        }

        graph.doRecursive(node => { node.relations = node.relations.reverse })
        return DecoderResult(graph, feats, score)
    }
}

