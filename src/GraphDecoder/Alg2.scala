package edu.cmu.lti.nlp.amr.GraphDecoder
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.FastFeatureVector._

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

class Alg2(options: Map[Symbol, String], featureNames: List[String], labelSet: Array[(String, Int)], connected: Boolean = true) extends Decoder {
    // Base class has defined:
    // val features: Features
    var features = new Features(options, featureNames, labelSet.map(_._1))

    private var inputSave: Input = _
    def input : Input = inputSave
    def input_= (i: Input) {
        inputSave = i
        features.input = i
        precomputeEdgeWeights
    }

    private var edgeWeights : Array[Array[Array[(String, Double)]]] = Array()

    def precomputeEdgeWeights() {
        // WARNING: THIS CODE ASSUMES THAT THE LAGRANGE MULTIPLIERS ARE SET TO ZERO
        // TODO: fix this so errors don't occur
        var graph = input.graph.get.duplicate
        val nodes : Array[Node] = graph.nodes.filter(_.name != None).toArray
        //val nonDistinctLabels = labelSet.toList.filter(x => x._2 > 1) // TODO: remove
        //val distinctLabels = labelSet.filter(x => x._2 == 1)  // TODO: remove
        //val nonDistinctLabels : Array[(String, Int)] = new Array(0)
        //val distinctLabels = labelSet
        //edgeWeightsDistict = weightMatrix(nodes, distinctLabels)
        //edgeWeightsND = weightMatrix(nodes, nonDistinctLabels)
        edgeWeights = weightMatrix(nodes, labelSet)
    }

    def weightMatrix(nodes: Array[Node], labels: Array[(String, Int)]) : Array[Array[Array[(String, Double)]]] = {
        //logger(1, "Computing edgeWeights")
        //logger(1, "featureNames = "+features.featureNames.toString)
        val edgeWeights : Array[Array[Array[(String, Double)]]] = nodes.map(x => Array.fill(0)(Array.fill(0)("",0.0)))
        for (i <- 0 until nodes.size) {
            edgeWeights(i) = nodes.map(x => Array.fill(0)(("",0.0)))
            for (j <- 0 until nodes.size) {
                if (i == j) {
                    edgeWeights(i)(j) = Array((":self", 0.0)) // we won't add this to the queue anyway, so it's ok
                } else {
                    edgeWeights(i)(j) = Array.fill(labelSet.size)(("", 0.0))
                    val feats = features.localFeatures(nodes(i), nodes(j))
                    //logger(0, "localFeatures("+nodes(i).concept+")("+nodes(j).concept+") = "+feats.toString)
                    features.weights.iterateOverLabels2(feats,
                        x => { //logger(3, "These should be equal: " + x.value.toString + " " + features.localScore(nodes(i), nodes(j), features.weights.labelset(x.labelIndex).toString));
                        //edgeWeights(i)(j)(x.labelIndex) = (features.weights.labelset(x.labelIndex), features.localScore(nodes(i), nodes(j), features.weights.labelset(x.labelIndex))) })
                        edgeWeights(i)(j)(x.labelIndex) = (features.weights.labelset(x.labelIndex), x.value) })
                        //logger(0, "edgeWeights("+nodes(i).concept+")("+nodes(j).concept+") = "+edgeWeights(i)(j).toList.sortBy(-_._2))
                }
            }
        }
        return edgeWeights
    }

    def decode(i: Input) : DecoderResult = { 
        input = i 
        decode 
    }

    def decode() : DecoderResult = {
        // Assumes that Node.relations has been setup correctly for the graph fragments
        var graph = input.graph.get.duplicate
        //logger(1, "Alg2 input graph: ")
        //logger(1, graph.printTriples(detail = 1)+"\n")
        //val nodes : Array[Node] = graph.nodes.filter.toArray
        val nodes : Array[Node] = graph.nodes.filter(_.name != None).toArray
        //logger(1, "nodes = "+nodes.map(x => x.id).toList.toString)
        //val nonDistinctLabels = labelSet.toList.filter(x => x._2 > 1) // TODO: remove
        val nonDistinctLabels : Array[(String, Int)] = new Array(0)
        //logger(1,"ndLabels = "+nonDistinctLabels.toList)
        //val distinctLabels = labelSet.filter(x => x._2 == 1)  // TODO: remove
        val distinctLabels = labelSet
        //logger(1, "labelSet = "+labelSet.toList.toString)
        //logger(1, "-- Alg2 Weights --\n"+features.weights.toString)
        //logger(0, "CA weights:\n" + features.weights.filter(x => x.startsWith("CA:C1")).toString)
        //logger(0, "CA:U weights:\n" + features.weights.filter(x => x.startsWith("CA:U_C1")).toString)

        // Each node is numbered by its index in 'nodes'
        // Each set is numbered by its index in 'setArray'
        // 'set' contains the index of the set that each node is assigned to
        // At the start each node is in its own set
        val set : Array[Int] = nodes.zipWithIndex.map(_._2)
        val setArray : Array[Set[Int]] = nodes.zipWithIndex.map(x => Set(x._2))
        def getSet(nodeIndex : Int) : Set[Int] = { setArray(set(nodeIndex)) }

        var score = 0.0
        var feats = new FeatureVector(features.weights.labelset)
        def addEdge(node1: Node, index1: Int, node2: Node, index2: Int, label: String, weight: Double, addRelation: Boolean = true) {
            if (!node1.relations.exists(x => ((x._1 == label) && (x._2.id == node2.id))) || !addRelation) { // Prevent adding an edge twice
                //logger(0, "Adding edge ("+node1.concept+", "+label +", "+node2.concept + ") with weight "+weight.toString)
                if (addRelation) {
                    node1.relations = (label, node2) :: node1.relations
                }
                feats += features.localFeatures(node1, node2, features.weights.labelToIndex(label))  // TODO: could speed this up (not use String for label)
                score += weight
            }
            //logger(2, "set = " + set.toList)
            //logger(2, "nodes = " + nodes.map(x => x.concept).toList)
            //logger(2, "setArray = " + setArray.toList)
            if (set(index1) != set(index2)) {   // If different sets, then merge them
                //logger(2, "Merging sets")
                getSet(index1) ++= getSet(index2)
                val set2 = getSet(index2)
                for (index <- set2) {
                    set(index) = set(index1)
                }
                set2.clear()
            }
            //logger(2, "set = " + set.toList)
            //logger(2, "nodes = " + nodes.map(x => x.concept).toList)
            //logger(2, "setArray = " + setArray.toList)
        }

        //logger(1, "Adding edges already there")
        val nodeIds : Array[String] = nodes.map(_.id)
        for { (node1, index1) <- nodes.zipWithIndex
              (label, node2) <- node1.relations } {
            //logger(2, "1: node1 = "+node1.concept+" "+node1.id)
            //logger(2, "1: node2 = "+node2.concept+" "+node2.id)
            if (nodeIds.indexWhere(_ == node2.id) != -1) {
                val index2 = nodeIds.indexWhere(_ == node2.id)
                addEdge(node1, index1, node2, index2, label, features.weights.dot(features.localFeatures(node1, node2, label)), addRelation=false)
            } else {
                val temp = features.localFeatures(node1, node2, label)
                feats += temp
                score += features.weights.dot(temp)
            }
        }

        //logger(2, "set = " + set.toList)
        //logger(2, "nodes = " + nodes.map(x => x.concept).toList)
        //logger(2, "setArray = " + setArray.toList)

        //logger(1, "Adding positive edges")
        val neighbors : Array[Array[(String, Double)]] = {
            for ((nodes2, index1) <- edgeWeights.zipWithIndex) yield {
                //logger(2, "index1 = "+index1.toString)
                val node1 = nodes(index1)
                for ((labelWeights, index2) <- nodes2.zipWithIndex) yield {
                    //logger(2, "index2 = "+index1.toString)
                    val node2 = nodes(index2)
                    // For node1 and node2, we will find the label with the highest weight
                    // However, to support LR decoding we will also add features.weights.dot(features.ffLRLabelWithId(node1, node2, x._1))
                    // The code used to be:
                    // val (label, weight) = labelWeights.map(x => (x._1, x._2 + features.weights.dot(features.ffLRLabelWithId(node1, node2, x._1)))).maxBy(_._2)
                    // but now with FastFeatureVector it is:
                    val (label, weight) = labelWeights.view.zipWithIndex.map(x => (x._1._1, x._1._2 + { val (f,v,_) = features.ffLRLabelWithId(node1, node2)(0); features.weights(f, Some(x._2)) * v.conjoined } )).maxBy(_._2)
                    //logger(2, "labelWeights = "+labelWeights.view.zipWithIndex.map(x => (x._1._1, x._1._2 + { val (f,v,_) = features.ffLRLabelWithId(node1, node2)(0); features.weights(f, Some(x._2)) * v.conjoined } )).toList)
                    if (weight > 0) {   // Add if positive
                        addEdge(node1, index1, node2, index2, label, weight)
                    }
                    (label, weight)
                }
            }
        }

/* ******** This code used to support nonDistinctLabels ************
        val neighbors : Array[Array[(String, Double)]] = {
            for ((node1, index1) <- nodes.zipWithIndex) yield {
                for ((node2, index2) <- nodes.zipWithIndex) yield {
                    if (index1 == index2) {
                        (":self", 0.0) // we won't add this to the queue anyway, so it's ok
                    } else {
                    val (label, weight) = distinctLabels.map(x => (x._1, features.localScore(node1, node2, x._1))).maxBy(_._2)
                    //logger(2,"distinctLabels = "+distinctLabels.map(x => (x._1, features.localScore(node1, node2, x._1))).sortBy(-_._2).toList.take(5)+"...")
                    //logger(2,"label = "+label)
                    //logger(2,"weight = "+weight)
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
                            logger(1, "1neighbors("+node1.concept+","+node2.concept+")="+label)
                            (label, weight)
                        } else {
                            logger(1, "2neighbors("+node1.concept+","+node2.concept+")="+label2)
                            (label2, weight2)
                        }
                    } else {
                        logger(1, "neighbors("+node1.concept+","+node2.concept+")="+label)
                        logger(1, "neighbors("+index1.toString+","+index2.toString+")="+label+" "+weight.toString)
                        (label, weight)
                    }
                    }
                }
            }
        } ************************************************** */

        // Uncomment to print neighbors matrix
        /*logger(0, "Neighbors matrix")
        for { (node1, index1) <- nodes.zipWithIndex
              ((label, weight), index2) <- neighbors(index1).zipWithIndex } {
            logger(0,"neighbors("+nodes(index1).concept+","+nodes(index2).concept+")="+label+" "+weight.toString)
        }*/

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
                    //logger(2, "set = " + set.toList)
                    //logger(2, "getSet(0)" + getSet(0))
                }
            }
        }

        //logger(1, "nodes = "+nodes.toList)
        //logger(1, "Alg2 returning graph: ")
        //logger(1, graph.printTriples(detail = 1)+"\n")
        if(nodes.size > 0) {
            if (features.rootFeatureFunctions.size != 0 && nodes.filter(node => !node.concept.startsWith("\"") && !node.concept.matches("[0-9].*")).size != 0) {
                graph.root = nodes.filter(node => !node.isConstant).map(x => (x, features.rootScore(x))).maxBy(_._2)._1
            } else {
                //logger(1, "Setting root to "+nodes(0).id)
                graph.root = nodes(0)
            }
            feats += features.rootFeatures(graph.root)
            score += features.rootScore(graph.root)

            nodes.map(node => { node.relations = node.relations.reverse })
            //if (connected) {
            //    graph.makeTopologicalOrdering()
            //}
        } else {
            graph = Graph.AMREmpty()
        }

        logger(1, "Alg2 returning score = " + score.toString)
        return DecoderResult(graph, feats, score)
    }
}

