package edu.cmu.lti.nlp.amr

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
import scala.util.parsing.combinator._

case class Span(var start: Int, var end: Int, var nodeIds: List[String], var words: String, var amr: Node) {
    def format() : String = {
        start.toString+"-"+end.toString+"|"+nodeIds.mkString("+")
    }
}

//case class AbstractSpan(words: String, amr: Node)

object Span {
    def readSpans(string: String, graph: Graph, sentence: Array[String]) {
        val spans = graph.spans
        spans.clear
        val SpanRegex = """([0-9]+)-([0-9]+)|(.*)""".r
        for (spanStr <- string.split(" ")) {
            //try {
                val SpanRegex(start, end, nodeStr) = spanStr
                val nodeIds = nodeStr.split("[+]").toList.sorted
                val words = getWords(start.toInt, end.toInt, sentence)
                val amr = getAmr(nodeIds, graph)
                spans += Span(start.toInt, end.toInt, nodeIds, words, amr)
            //} catch {
                // TODO: catch malformed input (Regex match error, or toInt err
            //}
        }
    }

    private def getWords(start: Int, end: Int, sentence: Array[String]) : String = {
        if(start >= end) {
            throw new RuntimeException("Span of improper length: "+start.toString+"-"+end.toString)
        }
        sentence.slice(start, end).mkString(" ")
    }

    private def getAmr(nodeIds: List[String], graph: Graph) : Node = {
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
        val node = graph.getNodeById(nodes(0))
        var unprocessed = nodes.tail
        // Node(var id: String, name: Option[String], concept: String, var relations: List[(String, Node)], var topologicalOrdering: List[(String, Node)], var variableRelations: List[(String, Var)], var alignment: Option[Int], var span: Option[Int])
        val myNode = Node(id = myId, name = node.name, concept = node.concept, relations = List[(String, Node)](), topologicalOrdering = List[(String, Node)](), variableRelations = List[(String, Var)](), alignment = None, span = None)
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

