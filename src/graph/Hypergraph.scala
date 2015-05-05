package edu.cmu.lti.nlp.amr.graph
//import edu.cmu.lti.nlp.amr._

import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable => m, immutable => i}  // m.Set, m.Map, i.Set, i.Map

import scala.reflect.ClassTag       // see http://stackoverflow.com/questions/16921168/scala-generic-method-no-classtag-available-for-t
import scala.annotation.tailrec

object Hypergraph {
    case class Node[N](get: N, index: Int)                              // Node type of the hypergraph
    case class Edge[N,E](dest: Node[N], src: Vector[Node[N]], get: E)   // Edge type of the hypergraph
    case class BareEdge[N,E](dest: N, src: Vector[N], get: E)           // Edge without indices
    case class SplitEdge[N,M,E](dest: Node[N], src: Vector[Node[M]], get: E)  // Edge type that is used when we are splitting states

    class Builder[N,E] {
        private val stateMap : m.Map[N,Int] = m.Map()
        private val states : ArrayBuffer[N] = ArrayBuffer()
        private var edges : List[Edge[N,E]] = List()

        def node(state: N) : Node[N] = {            // This is a semi-expensive operation (cache the result)
            if (!stateMap.contains(state)) {
                stateMap(state) = states.size
                states += state                     // append it
                return Node(state, states.size - 1)
            }
            return Node(state, stateMap(state))
        }
        def node(i: Int) : Node[N] = Node(states(i), i)
        def contains(state: N) : Boolean = stateMap.contains(state)
        def add(edge: Edge[N,E]) = edge :: edges                    // Add edge

        def toHypergraph : Hypergraph[N,E] = {
            new Hypergraph(states.view.zipWithIndex.map(x => Node(x._1, x._2)).toVector, edges)
        }
    }

/*  def fromEdges[N:ClassTag,E](nodes: Array[N], edges: List[Edge[Int,E]]) : Hypergraph[N,E] = {   // for why we need ClassTag, see http://stackoverflow.com/questions/16921168/scala-generic-method-no-classtag-available-for-t
        val bareNodes : m.Set[N] = m.Set()
        for { e <- bareEdges
              src <- e.src
            } {
                bareNodes += e.dest
                bareNodes += src
        }
        return new Hypergraph(bareNodes.toArray, bareEdges.toArray)
    } */
}

import Hypergraph.Node
import Hypergraph.Edge
import Hypergraph.BareEdge
import Hypergraph.SplitEdge

class Hypergraph[N,E](nodes: Vector[Node[N]], edges: List[Edge[N,E]]) {  // this class is immutable

    private val numNodes : Int = nodes.size
    //private lazy val _nodeArray : Array[Node[N]] = nodes.toArray   // TODO: remove (not needed)
    private lazy val _nodeToIndex : m.Map[N,Int] = {
        m.Map[N,Int]() ++ nodes.map(x => (x.get, x.index))
    }
    private lazy val _outgoingEdges : Array[List[Edge[N,E]]] = {
        val outEdges : Array[List[Edge[N,E]]] = Array.fill(numNodes)(List())
        for { e <- edges
              src <- e.src
            } {
                outEdges(src.index) = e :: outEdges(src.index)
        }
        for ((list, index) <- outEdges.zipWithIndex) {
            outEdges(index) = list.reverse
        }
        outEdges
    }
    private lazy val _incomingEdges : Array[List[Edge[N,E]]] = {
        val inEdges : Array[List[Edge[N,E]]] = Array.fill(numNodes)(List())
        for (e <- edges) {
            inEdges(e.dest.index) = e :: inEdges(e.dest.index)
        }
        for ((list, index) <- inEdges.zipWithIndex) {
            inEdges(index) = list.reverse
        }
        inEdges
    }
    //private lazy val _terminals : List[Node[N]] = nodes.view.filter(n => incomingEdges(n) == List() || !incomingEdges(n).contains((e : Edge[N,E]) => { e.src != List() })).toList  // TODO: remove (not needed)

    //private def outgoingEdges(srcIndex: Int) : List[Edge[N,E]] = _outgoingEdges(srcIndex)     // Not needed
    //private def incomingEdges(destIndex: Int) : List[Edge[N,E]] = _incomingEdges(destIndex)

    def node(n: N) : Node[N] = Node(n, _nodeToIndex(n))             // This is an expensive operation
    def node(i: Int) : Node[N] = nodes(i)
    /*def edge(e: Edge[N,E]) : Edge[N,E] = {                        // This is an expensive operation
        return Edge(node(e.dest), e.src.map(x => node(x)), e.get)
    }*/
    def outgoingEdges(src: Node[N]) : List[Edge[N,E]] = _outgoingEdges(src.index)
    def incomingEdges(dest: Node[N]) : List[Edge[N,E]] = _incomingEdges(dest.index)

    /*def toCdecFormat(goalNode: N => Boolean, edgeToString: Edge[N,E] => String) : String = {
        write(nodes.size + " " + edges.size)
        for(
    } */

    // TODO: add unioning operation for hypergraphs


    /********************** State splitting ***************/

    def splitStates[M,F](goalNode: N => Boolean,
                         splitFunction: (SplitEdge[N,M,E], M => Node[M]) => List[Edge[M,F]]
                        ) : Hypergraph[M,F] = {
        val goalNodes : List[Node[N]] = nodes.view.filter(n => goalNode(n.get)).toList
        val builder : Hypergraph.Builder[M,F] = new Hypergraph.Builder()
        val processedNodes : Array[Option[List[Node[M]]]] = Array.fill(numNodes)(None)
        for (goal <- goalNodes) {
            splitStates(goal, builder, splitFunction, processedNodes)
        }
        //return Hypergraph.fromEdges(edges.distinct)
        return builder.toHypergraph
    }

    def splitStates[M,F](node: Node[N],
                       builder: Hypergraph.Builder[M,F],
                       splitFunction: (SplitEdge[N,M,E], M => Node[M]) => List[Edge[M,F]],
                       processedNodes: Array[Option[List[Node[M]]]]) : List[Node[M]] = {
        // If we've processed this node before, return cached results
        if (processedNodes(node.index) != None) {
            return processedNodes(node.index).get
        }
        val myStates : m.Set[Int] = m.Set()
        for (edge <- incomingEdges(node)) {
            // recurse to nodes we haven't processed yet
            var sources : List[List[Node[M]]] = List()
            for (src <- edge.src) {
                    sources = splitStates(src, builder, splitFunction, processedNodes) :: sources
            }
            sources = sources.reverse
            // for each seq of source states, run splitNodes
            for { source <- getProduct(sources)     // Make sure that if it's passed and empty list, it returns List(List())
                  edge <- splitFunction(SplitEdge(edge.dest, source.toVector, edge.get), builder.node)    // TODO: get rid of .toVector
                } {
                    myStates += edge.dest.index
                    builder.add(edge)
            }
        }
        val states = myStates.map(i => builder.node(i)).toList
        processedNodes(node.index) = Some(states)
        return states
    }

    // see http://stackoverflow.com/questions/13567543/cross-product-of-arbitrary-number-of-lists-in-scala
    // TODO: make tail recursive, move to amr.package
    private def getProduct[M](input: List[List[M]]) : List[List[M]] = input match{
        case Nil => List(List())            // just in case you input an empty list
        case head :: Nil => head.map(_ :: Nil)
        case head :: tail => for (elem <- head; sub <- getProduct(tail)) yield elem::sub
    }
}

