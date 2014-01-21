// Copyright (c) 2014, Sam Thomson
package edu.cmu.lti.nlp.amr

import scala.annotation.tailrec

object CycleTester {
  /**
    Takes a directed graph, as a set of nodes and a map from node to its out-adjacent nodes,
    and determines whether the graph contains a cycle.
   */
  @tailrec
  def hasCycle[T](nodes: Traversable[T], outgoingEdges: Map[T, Traversable[T]]): Boolean = {
    if (outgoingEdges.isEmpty) {
      false
    } else {
      val oFirstLeaf: Option[T] = nodes.toIterator.find(v => outgoingEdges.getOrElse(v, Nil).isEmpty).headOption
      oFirstLeaf match {
        case None => true
        case Some(node) => {
          val removed = (outgoingEdges - node) map { case (k, v) => (k, v.toSet - node) }
          hasCycle(nodes.toSet - node, removed)
        }
      }
    }
  }

  def main(args: Array[String]) {
    val graphA = Map(
      1 -> Set(2),
      2 -> Set(3),
      3 -> Set(1)
    )
    println(hasCycle(1 to 3, graphA) + " should be true")

    val graphB = Map(
      1 -> Set(2, 3),
      2 -> Set(3)
    )
    println(hasCycle(1 to 3, graphB) + " should be false")

    val graphC = Map(
      1 -> Set(2),
      2 -> Set(3, 4),
      4 -> Set(5, 6),
      5 -> Set(6),
      6 -> Set(3)
    )
    println(hasCycle(1 to 6, graphC) + " should be false")

    val graphD = Map(
      1 -> Set(2),
      2 -> Set(3, 4),
      4 -> Set(5),
      5 -> Set(6),
      6 -> Set(3, 4)
    )
    println(hasCycle(1 to 6, graphD) + " should be true")
  }
}
