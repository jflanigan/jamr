package edu.cmu.lti.nlp.amr.graph
//import edu.cmu.lti.nlp.amr._

import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable => m, immutable => i}  // m.Set, m.Map, i.Set, i.Map

object Hypergraph {
    case class Node[N](get: N, idx: Int)
    case class Edge[N,E](dest: Node[N], src: Array[Node[N]], get: E)

    //def fromEdges(edges: Iterable[(N, List[N])])
}

import Hypergraph.Node
import Hypergraph.Edge

class Hypergraph[N,E] private (_nodes: Array[Node[N]], _edges: Array[Edge[N,E]]) {  // private constructor, see http://stackoverflow.com/questions/1730536/private-and-protected-constructor-in-scala
    private var _outgoingEdges : Array[Edge[N,E]] = _
    private var _incomingEdges : Array[Edge[N,E]] = _
    private var _nodeToIdx : m.Map[N,Int] = _

    private def outgoingEdges(srcIdx: Int) : Edge[N,E] = _outgoingEdges(srcIdx)
    private def incomingEdges(destIdx: Int) : Edge[N,E] = _incomingEdges(destIdx)

    def nodes : Iterator[Node[N]] = _nodes.toIterator
    def edges : Iterator[Edge[N,E]] = _edges.toIterator
    def getNode(node: N) : Node[N] = Node(node, _nodeToIdx(node))
    def outgoingEdges(src: Node[N]) : Edge[N,E] = outgoingEdges(src.idx)
    def incomingEdges(dest: Node[N]) : Edge[N,E] = incomingEdges(dest.idx)

    //def splitStates(split: (Node[N], Edge[E]) => (Node[M], Edge[F]) : Hypergraph[M,F] = {
    //}

}

