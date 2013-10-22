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

case class Graph(var root: Node, spans: ArrayBuffer[Span], getNodeById: Map[String, Node], getNodeByName: Map[String, Node]) {

    def duplicate : Graph = {
        // Makes a copy of the graph
        // Assumes that getNodeById exists and is properly set up (for 'nodes' call)
        val getNodeById2 : Map[String, Node] = Map()
        for (node <- nodes) {
            val Node(id, name, concept, relations, topologicalOrdering, variableRelations, alignment, spans) = node
            getNodeById2(id) = Node(id, name, concept, List(), List(), List(), alignment, spans)
        }
        for (node <- nodes) {
            val node2 = getNodeById2(node.id)
            node2.relations = node.relations.map(x => (x._1, getNodeById2(x._2.id)))
            node2.topologicalOrdering = node.topologicalOrdering.map(x => (x._1, getNodeById2(x._2.id)))
            node2.variableRelations = node.variableRelations.map(x => (x._1, Var(getNodeById2(x._2.node.id), x._2.name)))
        }
        val getNodeByName2 = getNodeByName.map(x => (x._1, getNodeById2(x._2.id)))
        return Graph(getNodeById2(root.id), spans.clone, getNodeById2, getNodeByName2)
    }

    def clearEdges() {
        // Initializes the graph from the spans (effectively clearing the edges)
        // Sets relations, but leaves topologicalOrdering and variableRelations blank

        var currentId = 0
        getNodeById.clear
        getNodeByName.clear

        def addRec(node: Node) : (Node, List[String]) = {
            var ids = List(currentId.toString)
            val node2 = Node(node.id, node.name, node.concept, List(), List(), List(), node.alignment, node.spans)
            var relations = List[(String, Node)]()
            getNodeById(currentId.toString) = node2
            node.name match {
                case Some(name) => { getNodeByName(name) = node2 }
                case None => Unit
            }
            currentId += 1
            for ((relation, child) <- node.topologicalOrdering) {
                val result = addRec(child)
                ids = ids ::: result._2
                relations = (relation, result._1) :: relations
            }
            node2.relations = relations.reverse
            return (node2, ids)
        }

        // Add all the non-coref spans
        for (span <- spans if !span.coRef) {
            span.nodeIds = addRec(span.amr)._2
        }

        // Set the span ids for the coref spans correctly
        for (span <- spans if span.coRef) {
            span.nodeIds = List()
        }

        for ((id, node) <- getNodeById) {
            for { spanIndex <- node.spans
                  val span = spans(spanIndex)
                  if span.coRef
                } {
                span.nodeIds = span.nodeIds ::: List(id)
            }
        }
    }

    def printTriples(detail: Int = 1) {
        def name(node: Node) : String = {
            node.name match {
                case None => ""
                case Some(n) => n + " / "
            }
        }

        val Relation = """:?(.*)""".r

        for { node1 <- nodes
              (Relation(relation), node2) <- node1.relations
            } {
            detail match {
                case 0 => println(relation + "(" + node1.concept + ", " + node2.concept + ")")
                case _ => println("(" + name(node1) + node1.concept + ", " + name(node2) + node2.concept + ", " + relation + ")")
            }
        }
    }

    def loadSpans(spanStr: String, sentence: Array[String]) = {
        assert(spans.size == 0, "This code does not support re-loading the spans")
        //spans.clear
        val SpanRegex = """([*]?)([0-9]+)-([0-9]+)\|(.*)""".r   // TODO: move to Span
        for (spanStr <- spanStr.split(" ")) {
            try {
                val SpanRegex(corefStr, start, end, nodeStr) = spanStr
                val nodeIds = nodeStr.split("[+]").toList.sorted
                val words = SpanLoader.getWords(start.toInt, end.toInt, sentence)   // TODO: use addSpan function
                val amr = SpanLoader.getAmr(nodeIds, this)
                val coref = corefStr match {
                    case "*" => true
                    case "" => false
                }
                spans += Span(start.toInt, end.toInt, nodeIds, words, amr, coref)
                for (id <- nodeIds) {
                    getNodeById(id).addSpan(spans.size-1, coref)
                }
            } catch {
                // TODO: catch malformed input (Regex match error, or toInt err)
                case e : Throwable => logger(1, "****************** MALFORMED SPAN: "+spanStr)
            }
        }
    }

    def addSpan(start: Int, end: Int, nodeIds: List[String], coRef: Boolean, sentence: Array[String]) {
        val span = Span(start, end, nodeIds, sentence.slice(start, end).mkString(" "), SpanLoader.getAmr(nodeIds, this), coRef)
        addSpan(span)
    }

    def addSpan(span: Span) {
        spans += span
        for (id <- span.nodeIds) {
            getNodeById(id).addSpan(spans.size-1, span.coRef)
        }
    }

    def updateSpan(spanIndex: Int, start: Int, end: Int, nodeIds: List[String], coRef: Boolean, sentence: Array[String]) {
        //println("new nodes = "+nodeIds.toString)
        for (id <- spans(spanIndex).nodeIds) {
            //println(getNodeById(id).spans)
            getNodeById(id).spans -= spanIndex
        }
        val span = Span(start, end, nodeIds, sentence.slice(start, end).mkString(" "), SpanLoader.getAmr(nodeIds, this), coRef)
        spans(spanIndex) = span
        for (id <- nodeIds) {
            getNodeById(id).addSpan(spanIndex, coRef)
            //println(getNodeById(id).spans)
        }
    }

    def updateSpan(spanIndex: Int, start: Int, end: Int, sentence: Array[String]) {
        // Update start, end
        val span = spans(spanIndex)
        updateSpan(spanIndex, start, end, span.nodeIds, span.coRef, sentence)
    }

    def updateSpan(spanIndex: Int, nodeIds: List[String], sentence: Array[String]) {
        // Update nodeIds
        val span = spans(spanIndex)
        updateSpan(spanIndex, span.start, span.end, nodeIds, span.coRef, sentence)
    }

    def updateSpan(spanIndex: Int, coRef: Boolean, sentence: Array[String]) {
        // Update coRef indicator
        val span = spans(spanIndex)
        updateSpan(spanIndex, span.start, span.end, span.nodeIds, coRef, sentence)
    }

    def nodes : Iterator[Node] = {
        return getNodeByName.valuesIterator
    }

    def doRecursive(f: (Node) => Unit, node: Node = root) {
        f(node)
        for ((_,child) <- node.topologicalOrdering) {
            doRecursive(f, child)
        }
    }

/* Very cool recursive function TODO: use this one instead
    def doRecursive(parentMessage: T, node: Node, f: (Node, T) => T, g: List[T] => R ) : R = {
        val message = f(parentMessage, node)
        return g(node.topologicalOrdering.map(x => doRecursive(message, x.2, f, g)).toList)
    }
*/

    private def makeIds(node: Node = root, id: List[Int] = List(0)) {
        // Sets the node.id field for every node in the graph according to the topologicalOrdering
        // For example "0" is the root and "0.1" is the 2nd child of the root
        // Assumes that a topological ordering already exists (node.topologicalOrdering is non-empty)
        if (node != root) {
            getNodeById.clear
        }
        node.id = id.mkString(".")
        getNodeById += (node.id -> node)
        for (((_,child), i) <- node.topologicalOrdering.zipWithIndex) {
            makeIds(child, id ::: List(i))
        }
    }

    private def makeVariables(node: Node = root) {
        // Populate the getNodeByName map
        // Assumes a topologicalOrdering exists and node.name is set
        if (node != root) {
            getNodeByName.clear
        }
        if (node.name != None) {
            val Some(name) = node.name
            if (getNodeByName.contains(name)) {
                throw new RuntimeException("duplicate variable name: " + name)
            } else {
                getNodeByName += (name -> node)
                for ((_,child) <- node.topologicalOrdering) {
                    makeVariables(child)
                }
            }
        }
    }

    private def unifyVariables(node: Node = root) {
        // Postcondition: Unify variables, remove variables from node.topologicalOrdering,
        // and populate node.relations and node.variableRealtions attributes for each node
        // Precondition: topologicalOrdering was filled in by the graph parser
        // and that makeVariables has already been called
        val relations = node.topologicalOrdering
        node.relations = List[(String, Node)]()
        node.topologicalOrdering = List[(String, Node)]()
        node.variableRelations = List[(String, Var)]()
        for ((relation, child) <- relations) {
            // figure out if child is a node, or a variable
            if (child.name == None && getNodeByName.contains(child.concept)) { // variables have concepts, but no names
                // child is a variable
                val varName = child.concept
                val actualNode = getNodeByName(varName)
                node.relations = node.relations ::: List((relation, actualNode))
                node.variableRelations = node.variableRelations ::: List((relation, Var(actualNode, varName)))
            } else {
                // child is a legit node
                node.relations = node.relations ::: List((relation, child))
                node.topologicalOrdering = node.topologicalOrdering ::: List((relation, child))
                unifyVariables(child)
            }
        }
    }
}

object Graph {
    private class GraphParser extends JavaTokenParsers {
        // Parser implementation for parsing AMR graphs
        def variable : Parser[String] = """[a-zA-Z0-9]+""".r
        def concept : Parser[String] = """([a-zA-Z0-9.-]+)|("[^"]+")""".r
        //def concept : Parser[String] = """\S+""".r
        def relationStr : Parser[String] = """:[a-zA-Z0-9-]+""".r
        //def variable : Parser[String] = """\S+""".r
        //def concept : Parser[String] = """\S+""".r
        //def relationStr : Parser[String] = """:\S+""".r
        def relation : Parser[(String, Node)] = relationStr~node ^^ {
            case relationStr~node => (relationStr, node)
        }
        def relations : Parser[List[(String, Node)]] = rep(relation)
        def internalNode : Parser[Node] = "("~>variable~"/"~concept~relations<~")" ^^ {
            case variable~"/"~concept~relations => Node("", Some(variable), concept, List[(String, Node)](), relations, List[(String, Var)](), None, ArrayBuffer[Int]())
        }
        def terminalNode : Parser[Node] = concept ^^ { 
            case concept => Node("", None, concept, List[(String, Node)](), List[(String, Node)](), List[(String, Var)](), None, ArrayBuffer[Int]())
        }
        def node : Parser[Node] = terminalNode | internalNode
    }

    private val parser = new GraphParser()

    def load(iterator: Iterator[String]) : Iterator[Graph] = {
        for (line <- iterator) yield {
            parse(line)
        }
    }

    def parse(amr: String) : Graph = {
        val graph = parser.parseAll(parser.node, amr) match {
            case parser.Success(e, _) => Graph(e, new ArrayBuffer[Span](), Map[String, Node](), Map[String, Node]())
        }
        graph.makeVariables()
        graph.unifyVariables()
        graph.makeIds()
        return graph
    }

    def empty() : Graph = { parse("(n / none)") }
}

