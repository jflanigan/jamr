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
import scala.collection.mutable.PriorityQueue
import Double.{NegativeInfinity => minusInfty}

case class GraphObj(graph: Graph,
                    nodes: Array[Node], // usually 'nodes' is graph.nodes.filter(_.name != None).toArray
                    features: Features,
                    var set: Array[Int],
                    var setArray: Array[Set[Int]],
                    var score: Double = 0.0,
                    var feats: FeatureVector = new FeatureVector()) {

    // GraphObj is an object to keep track of the connectivity of the graph as edges are added to the graph.
    // It is code that was factored out of Alg2.  It is now also used in Alg1.

    // The two main functions are:
    // def connected : Boolean      Determines if the graph is connected
    // def addEdge()                Adds an edge to the graph, keeping track of connectivity

    // Each node is numbered by its index in 'nodes'
    // Each set is numbered by its index in 'setArray'
    // 'set' contains the index of the set that each node is assigned to
    // At the start each node is in its own set

    def this(graph: Graph, nodes: Array[Node], features: Features) = this(graph, nodes, features, nodes.zipWithIndex.map(_._2).toArray, nodes.zipWithIndex.map(x => Set(x._2)).toArray)

    def getSet(nodeIndex : Int) : Set[Int] = { setArray(set(nodeIndex)) }

    def connected : Boolean = if(set.size > 0) { getSet(0).size == set.size } else { true }

    def addEdge(node1: Node, index1: Int, node2: Node, index2: Int, label: String, weight: Double, addRelation: Boolean = true) {
        if (!node1.relations.exists(x => ((x._1 == label) && (x._2.id == node2.id))) || !addRelation) { // Prevent adding an edge twice
            logger(1, "Adding edge ("+node1.concept+", "+label +", "+node2.concept + ") with weight "+weight.toString)
            if (addRelation) {
                node1.relations = (label, node2) :: node1.relations
            }
            feats += features.localFeatures(node1, node2, label)
            score += weight
        }
        //logger(1, "set = " + set.toList)
        //logger(1, "nodes = " + nodes.map(x => x.concept).toList)
        //logger(1, "setArray = " + setArray.toList)
        if (set(index1) != set(index2)) {   // If different sets, then merge them
            //logger(1, "Merging sets")
            getSet(index1) ++= getSet(index2)
            val set2 = getSet(index2)
            for (index <- set2) {
                set(index) = set(index1)
            }
            set2.clear()
        }
        //logger(1, "set = " + set.toList)
        //logger(1, "nodes = " + nodes.map(x => x.concept).toList)
        //logger(1, "setArray = " + setArray.toList)
    }

    def log {
        logger(1, "set = " + set.toList)
        logger(1, "nodes = " + nodes.map(x => x.concept).toList)
        logger(1, "setArray = " + setArray.toList)
    }

    logger(1, "Adding edges already there")
    val nodeIds : Array[String] = nodes.map(_.id).toArray
    for { (node1, index1) <- nodes.zipWithIndex
          (label, node2) <- node1.relations } {
        if (nodeIds.indexWhere(_ == node2.id) != -1) {
            val index2 = nodeIds.indexWhere(_ == node2.id)
            addEdge(node1, index1, node2, index2, label, features.localScore(node1, node2, label), addRelation=false)
        } else {
            feats += features.localFeatures(node1, node2, label)
            score += features.localScore(node1, node2, label)
        }
    }

}

