package edu.cmu.lti.nlp.amr.graph
//import edu.cmu.lti.nlp.amr._

import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable => m, immutable => i}  // m.Set, m.Map, i.Set, i.Map

import scala.reflect.ClassTag       // see http://stackoverflow.com/questions/16921168/scala-generic-method-no-classtag-available-for-t
import scala.annotation.tailrec

object Hypergraph {
    case class Node[N](get: N, index: Int)                              // Node type of the hypergraph
    case class Edge[N,E](dest: Node[N], src: Vector[Node[N]], get: E)   // Edge type of the hypergraph
    case class BareEdge[N,E](dest: N, src: Vector[N], get: E)           // Edge type that is used to create the hypergraph
    case class SplitEdge[N,M,E](dest: Node[N], src: Vector[M], get: E)  // Edge type that is used when we are splitting states

    def fromEdges[N:ClassTag,E](bareEdges: Iterable[BareEdge[N,E]]) : Hypergraph[N,E] = {   // for why we need ClassTag, see http://stackoverflow.com/questions/16921168/scala-generic-method-no-classtag-available-for-t
        val bareNodes : m.Set[N] = m.Set()
        for { e <- bareEdges
              src <- e.src
            } {
                bareNodes += e.dest
                bareNodes += src
        }
        return new Hypergraph(bareNodes.toArray, bareEdges.toArray)
    }
}

import Hypergraph.Node
import Hypergraph.Edge
import Hypergraph.BareEdge
import Hypergraph.SplitEdge

class Hypergraph[N,E] private (_bareNodes: Array[N], _bareEdges: Array[BareEdge[N,E]]) {  // private constructor, see http://stackoverflow.com/questions/1730536/private-and-protected-constructor-in-scala
    private val _nodes: Array[Node[N]] = {
        _bareNodes.zipWithIndex.map(x => Node(x._1, x._2))
    }
    private val _nodeToIndex : m.Map[N,Int] = {
        m.Map[N,Int]() ++ _nodes.map(x => (x.get, x.index))
    }

    private val _edges: Array[Edge[N,E]] = {    // This is an expensive operation
        _bareEdges.map(x => edge(x))
    }
    private val _outgoingEdges : Array[List[Edge[N,E]]] = {
        val outEdges : Array[List[Edge[N,E]]] = _nodes.map(x => List())
        for { e <- _edges
              src <- e.src
            } {
                outEdges(src.index) = e :: outEdges(src.index)
        }
        for ((list, index) <- outEdges.zipWithIndex) {
            outEdges(index) = list.reverse
        }
        outEdges
    }
    private val _incomingEdges : Array[List[Edge[N,E]]] = {
        val inEdges : Array[List[Edge[N,E]]] = _nodes.map(x => List())
        for (e <- _edges) {
            inEdges(e.dest.index) = e :: inEdges(e.dest.index)
        }
        for ((list, index) <- inEdges.zipWithIndex) {
            inEdges(index) = list.reverse
        }
        inEdges
    }
    private val _terminals : List[Node[N]] = _nodes.filter(n => incomingEdges(n) == List() || !incomingEdges(n).contains((e : Edge[N,E]) => { e.src != List() })).toList

    //private def outgoingEdges(srcIndex: Int) : List[Edge[N,E]] = _outgoingEdges(srcIndex)     // Not needed
    //private def incomingEdges(destIndex: Int) : List[Edge[N,E]] = _incomingEdges(destIndex)

    def nodes : Iterator[Node[N]] = _nodes.toIterator
    def edges : Iterator[Edge[N,E]] = _edges.toIterator
    def node(n: N) : Node[N] = Node(n, _nodeToIndex(n))             // This is an expensive operation
    def edge(e: BareEdge[N,E]) : Edge[N,E] = {                      // This is an expensive operation
        return Edge(node(e.dest), e.src.map(x => node(x)), e.get)
    }
    def outgoingEdges(src: Node[N]) : List[Edge[N,E]] = _outgoingEdges(src.index)
    def incomingEdges(dest: Node[N]) : List[Edge[N,E]] = _incomingEdges(dest.index)

/*
    def splitStates[M](goalNode: N => Boolean,
                       split: SplitEdge[N,M,E] => BareEdge[M,E]) : Hypergraph[M,E] = {
        val nodesToProcess : List[(Node[N], SplitEdge[N,M,E])] = List()
        for (
        return splitStates(_terminals.map(e => SplitEdge(e.dest, List(), e.get)), split)
    } */

    def splitStates[M:ClassTag](goalNode: N => Boolean,
                                split: SplitEdge[N,M,E] => BareEdge[M,E]) : Hypergraph[M,E] = {
        val goalNodes : List[Node[N]] = _terminals.filter(n => goalNode(n.get))
        val splitNodes : m.Map[Int, List[M]] = m.Map()
        if (goalNodes.size == 1) {
            return Hypergraph.fromEdges(splitStates(goalNodes(0), split, splitNodes)._2)
        }
        var edges : List[BareEdge[M,E]] = List()
        for (goal <- goalNodes) {
            edges = splitStates(goal, split, splitNodes)._2 ::: edges
        }
        return Hypergraph.fromEdges(edges.distinct)
    }

    def splitStates[M](node: Node[N],
                       split: SplitEdge[N,M,E] => BareEdge[M,E],
                       splitNodes: m.Map[Int, List[M]]) : (List[M], List[BareEdge[M,E]]) = {
        if (splitNodes.contains(node.index)) {
            return (splitNodes(node.index), List())
        }
        var edges : List[BareEdge[M,E]] = List()
        val myStates : m.Set[M] = m.Set()
        for (edge <- incomingEdges(node)) {
            var sources : List[List[M]] = List()
            for { src <- edge.src
                  (newStates, newEdges) = splitStates(src, split, splitNodes)
                } {
                    edges = newEdges ::: edges
                    sources = newStates :: sources
            }
            sources = sources.reverse
            // for each seq of source states, run splitNodes
            for (source <- getProduct(sources)) {    // Make sure that if it's passed and empty list, it returns List(List())
                val newEdge : BareEdge[M,E] = split(SplitEdge(edge.dest, source.toVector, edge.get))    // TODO: get rid of toVector
                myStates += newEdge.dest
                edges = newEdge :: edges
            }
        }
        splitNodes(node.index) = myStates.toList
        return (splitNodes(node.index), edges)
    }

    // see http://stackoverflow.com/questions/13567543/cross-product-of-arbitrary-number-of-lists-in-scala
    // TODO: make tail recursive
    private def getProduct[M](input: List[List[M]]) : List[List[M]] = input match{
        case Nil => List(List())            // just in case you input an empty list
        case head :: Nil => head.map(_ :: Nil)
        case head :: tail => for (elem <- head; sub <- getProduct(tail)) yield elem::sub
    }

/*
    @tailrec
    def splitStates[M,F](processedNodes: i.Set[Int],
                         processedEdges: List[BareEdge[M,E]] = List(),
                         nodesToProcess: List[(Node[N], SplitEdge[N,M,E])],
                         split: SplitEdge[N,M,E] => BareEdge[M,E]) : List[BareEdge[M,E]] = {
        if (nodesToProcess == List()) {
            return processedEdges
        }
        var myProcessedNodes : i.Set[Int] = processedNodes
        var myProcessedEdges : List[Node[M]] = processedEdges
        var futureNodes : List[(Node[N], SplitEdge[N,M,E])] = List()
        for ((node, edge) <- nodesToProcess) {
            val newEdge : BareEdge[M,F] = split(edge)
            myProcessedEdges = newEdge :: myProcessedEdges
            if (!myProcessedNodes.contains(node.index)) {
                myProcessedNodes += node.index
                for (edge <- outgoingEdges(node)) {
                    futureNodes = (node, SplitEdge(edge.dest, edge
                }
            }
        }
        return splitStates(myProcessedEdges, futureNodes, split)
    } */

    /*def toCdecFormat(goalNode: N => Boolean, edgeToString: Edge[N,E] => String) : String = {
        write(_nodes.size + " " + _edges.size)
        for(
    } */

}

