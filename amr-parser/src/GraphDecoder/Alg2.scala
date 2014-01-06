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

class Alg2(featureNames: List[String], labelSet: Array[(String, Int)], connected: Boolean = true)
    extends Decoder(featureNames) {
    // Base class has defined:
    // val features: Features

    def decode(input: Input) : DecoderResult = {
        // Assumes that Node.relations has been setup correctly for the graph fragments
        var graph = input.graph.get.duplicate
        features.input = input
        //val nodes : Array[Node] = graph.nodes.filter.toArray
        val nodes : Array[Node] = graph.nodes.filter(_.name != None).toArray
        //val nonDistinctLabels = labelSet.toList.filter(x => x._2 > 1) // TODO: remove
        val nonDistinctLabels : Array[(String, Int)] = new Array(0)
        //logger(2,"ndLabels = "+nonDistinctLabels.toList)
        //val distinctLabels = labelSet.filter(x => x._2 == 1)  // TODO: remove
        val distinctLabels = labelSet

        // Each node is numbered by its index in 'nodes'
        // Each set is numbered by its index in 'setArray'
        // 'set' contains the index of the set that each node is assigned to
        // At the start each node is in its own set
        val set : Array[Int] = nodes.zipWithIndex.map(_._2)
        val setArray : Array[Set[Int]] = nodes.zipWithIndex.map(x => Set(x._2))
        def getSet(nodeIndex : Int) : Set[Int] = { setArray(set(nodeIndex)) }

        var score = 0.0
        var feats = new FeatureVector()
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

        //logger(1, "Adding edges already there")
        val nodeIds : Array[String] = nodes.map(_.id)
        for { (node1, index1) <- nodes.zipWithIndex
              (label, node2) <- node1.relations } {
            //logger(1, "1: node1 = "+node1.concept+" "+node1.id)
            //logger(1, "1: node2 = "+node2.concept+" "+node2.id)
            if (nodeIds.indexWhere(_ == node2.id) != -1) {
                val index2 = nodeIds.indexWhere(_ == node2.id)
                addEdge(node1, index1, node2, index2, label, features.localScore(node1, node2, label), addRelation=false)
            } else {
                feats += features.localFeatures(node1, node2, label)
                score += features.localScore(node1, node2, label)
            }
        }

        //logger(1, "set = " + set.toList)
        //logger(1, "nodes = " + nodes.map(x => x.concept).toList)
        //logger(1, "setArray = " + setArray.toList)

        //logger(1, "Adding positive edges")
        val neighbors : Array[Array[(String, Double)]] = {
            for ((node1, index1) <- nodes.zipWithIndex) yield {
                for ((node2, index2) <- nodes.zipWithIndex) yield {
                    if (index1 == index2) {
                        (":self", 0.0) // we won't add this to the queue anyway, so it's ok
                    } else {
                    val (label, weight) = distinctLabels.map(x => (x._1, features.localScore(node1, node2, x._1))).maxBy(_._2)
                    //logger(1,"distinctLabels = "+distinctLabels.map(x => (x._1, features.localScore(node1, node2, x._1))).sortBy(-_._2).toList.take(5)+"...")
                    //logger(1,"label = "+label)
                    //logger(1,"weight = "+weight)
                    val ndLabels = nonDistinctLabels.map(x => (x._1, features.localScore(node1, node2, x._1))).filter(x => x._2 > 0 && x._1 != label)
                    //logger(2,"ndLabels = "+nonDistinctLabels.map(x => (x._1, features.localScore(node1, node2, x._1))))
                    //logger(2,"ndLabels = "+ndLabels.toList)
                    ndLabels.filter(_._2 > 0).map(x => addEdge(node1, index1, node2, index2, x._1, x._2))
                    if (weight > 0) {   // Add all positive weights
                        addEdge(node1, index1, node2, index2, label, weight)
                    }
                    if (ndLabels.size > 0) {
                        val (label2, weight2) = ndLabels.maxBy(_._2)
                        if (weight > weight2) {
                            //logger(1, "1neighbors("+node1.concept+","+node2.concept+")="+label)
                            (label, weight)
                        } else {
                            //logger(1, "2neighbors("+node1.concept+","+node2.concept+")="+label2)
                            (label2, weight2)
                        }
                    } else {
                        //logger(1, "neighbors("+node1.concept+","+node2.concept+")="+label)
                        //logger(1, "neighbors("+index1.toString+","+index2.toString+")="+label+" "+weight.toString)
                        (label, weight)
                    }
                    }
                }
            }
        }

        //logger(1, "Neighbors matrix")
        for { (node1, index1) <- nodes.zipWithIndex
              ((label, weight), index2) <- neighbors(index1).zipWithIndex } {
            //logger(1,"neighbors("+index1.toString+","+index2.toString+")="+label+" "+weight.toString)
        }

        // Add negative weights to the queue
        //logger(1, "Adding negative edges")
        val queue = new PriorityQueue[(Double, Int, Int, String)]()(Ordering.by(x => x._1))
        if (connected && set.size != 0 && getSet(0).size != nodes.size) {
            for { (node1, index1) <- nodes.zipWithIndex
                  ((label, weight), index2) <- neighbors(index1).zipWithIndex
                  if index1 != index2 && weight <= 0 && set(index1) != set(index2) } {
                queue.enqueue((weight, index1, index2, label))
            }
        }

        // Kruskal's algorithm
        if (connected) {    // if we need to produce a connected graph
            //logger(1, "queue = " + queue.toString)
            //logger(1, "set = " + set.toList)
            //logger(1, "nodes = " + nodes.map(x => x.concept).toList)
            //logger(1, "setArray = " + setArray.toList)
            while (set.size != 0 && getSet(0).size != nodes.size) {
                //logger(2, queue.toString)
                val (weight, index1, index2, label) = queue.dequeue
                if (set(index1) != set(index2)) {
                    addEdge(nodes(index1), index1, nodes(index2), index2, label, weight)
                }
            }
        }
        
        //logger(1, "nodes = "+nodes.toList)
        if(nodes.size > 0) {
            if (features.rootFeatureFunctions.size != 0) {
                graph.root = nodes.map(x => (x, features.rootScore(x))).maxBy(_._2)._1
            } else {
                //logger(1, "Setting root to "+nodes(0).id)
                graph.root = nodes(0)
            }
            feats += features.rootFeatures(graph.root)

            nodes.map(node => { node.relations = node.relations.reverse })
            if (connected) {
                graph.makeTopologicalOrdering()
            }
        } else {
            graph = Graph.empty()
        }

        return DecoderResult(graph, feats, score)
    }
}

