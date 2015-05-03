package edu.cmu.lti.nlp.amr.graph
//import edu.cmu.lti.nlp.amr._

import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable => m, immutable => i}  // m.Set, m.Map, i.Set, i.Map

import scala.reflect.ClassTag       // see http://stackoverflow.com/questions/16921168/scala-generic-method-no-classtag-available-for-t

object Hypergraph {
    case class Node[N](get: N, index: Int)
    case class Edge[N,E](dest: Node[N], src: Vector[Node[N]], get: E)
    case class BareEdge[N,E](dest: N, src: Vector[N], get: E)

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

    //def splitStates[M,F](split: Edge[N,E] => (Node[M], Edge[M,F]) : Hypergraph[M,F] = {
    //}

}

