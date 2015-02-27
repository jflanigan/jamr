package edu.cmu.lti.nlp.amr

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

/************************************* Span Loader *******************************

This object contains functions usefull for loading the spans into a graph 
from a span string.  They are used in Graph.loadSpans.

Example span string: "1-2|0 0-1|0.0 2-3|0.1 4-5|0.2"

*********************************************************************************/

object SpanLoader {
    def toWordMap(spans: ArrayBuffer[Span], sentence: Array[String], assertNoOverlap: Boolean = false) : Array[ArrayBuffer[Int]] = {
        // returns an array that gives the span index of each word
        val wordToSpan : Array[ArrayBuffer[Int]] = Array.fill[ArrayBuffer[Int]](sentence.size)(ArrayBuffer.empty[Int])
        for ((span, spanIndex) <- spans.zipWithIndex) {
            for (i <- Range(span.start, span.end)) {
                if (assertNoOverlap) {
                    assert(wordToSpan(i) == 0, "Overlapping spans")
                }
                if (wordToSpan(i).size > 0) {
                    logger(0,"****************** WARNING: Overlapping spans **********************")
                    logger(0,"Word index = "+i.toString)
                    logger(0,"Span start = "+span.start+" Span end = "+span.end)
                }
                if (!spans(spanIndex).coRef) {
                    wordToSpan(i).+=:(spanIndex)    // prepend if not coRef
                } else {
                    wordToSpan(i) += spanIndex      // append if coRef
                }
            }
        }
        return wordToSpan
    }

    def getWords(start: Int, end: Int, sentence: Array[String]) : String = {
        if(start >= end) {
            throw new RuntimeException("Span of improper length: "+start.toString+"-"+end.toString)
        }
        sentence.slice(start, end).mkString(" ")
    }

    def getAmr(nodeIds: List[String], graph: Graph) : Node = {
        // Get the amr corresponding to the span
        // Assumes that the node ids are an in-order traversal of the graph fragment according to topologicalOrdering (and connected)
        if (nodeIds.size == 0) {
            throw new RuntimeException("Span with no aligned nodes")
        }
        val (amr, rest) = getAmr(nodeIds, graph, "0")
        if (rest.size > 0) {
            throw new RuntimeException("Cannot find connected AMR fragment in the topological ordering: "+nodeIds.mkString("+"))
        }
        return amr
    }

    private def getAmr(nodes: List[String], graph: Graph, myId: String) : (Node, List[String]) = {
        // TODO: maybe this function does not actually need graph
        val node = try {
            graph.getNodeById(nodes(0))
        } catch {
            case e : Throwable => throw new RuntimeException("Cannot find node ["+nodes(0)+"]")
        }
        var unprocessed = nodes.tail
        // Node(var id: String, name: Option[String], concept: String, var relations: List[(String, Node)], var topologicalOrdering: List[(String, Node)], var variableRelations: List[(String, Node)], var alignment: Option[Int], var span: Option[Int])
        val myNode = Node(id = node.id, name = node.name, concept = node.concept, relations = List[(String, Node)](), topologicalOrdering = List[(String, Node)](), variableRelations = List[(String, Node)](), alignment = node.alignment, spans = node.spans)
        var childNumber = 0
        var done = false
        while (unprocessed.size > 0 && !done) {
            val childId = unprocessed(0)
            val topologicalRelations = node.topologicalOrdering.map(x => (x._2.id,x._1)).toMap
            if (topologicalRelations contains childId) {
                val relation = topologicalRelations(childId)
                val (childNode, rest) = getAmr(unprocessed, graph, myId+"."+childNumber.toString)
                myNode.relations = (relation, childNode) :: myNode.relations
                myNode.topologicalOrdering = (relation, childNode) :: myNode.topologicalOrdering
                childNumber += 1
                unprocessed = rest
            } else {
                done = true
            }
        }
        node.relations = node.relations.reverse
        node.topologicalOrdering = node.topologicalOrdering.reverse
        return (myNode, unprocessed)
    }
}

