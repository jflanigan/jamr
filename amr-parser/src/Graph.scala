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

case class Var(node: Node, name: String)

case class Node(var id: String, name: Option[String], concept: String, var relations: List[(String, Node)], var topologicalOrdering: List[(String, Node)], var variableRelations: List[(String, Var)], var alignment: Option[Int], var spans: ArrayBuffer[Int] /* TODO: change to something immutable (ie List) Interacts if a span gets copied from this span */) {

    def children: List[(String, Node)] = topologicalOrdering    // property Ch 18.2 stairway book
    def children_= (c: List[(String, Node)]) {
        topologicalOrdering = c
    }

    def addSpan(span: Int, coRef: Boolean){
        if (coRef) {
            spans += span
        } else {
            if (spans.size > 0) {
                println("WARNING ADDING ANOTHER SPAN TO NODE "+id)
                println(spans.toString+" + "+span.toString)
            }
            spans.+=:(span) // prepend
        }
    }

    def someSpan() : Option[Int] = {
        if (spans.isEmpty) {
            None
        } else {
            Some(spans(0))
        }
    }

    override def toString() : String = {
        prettyString(0, false)
    }

    def prettyString(detail: Int, pretty: Boolean) : String = {
    // Detail 0: Least detail. No variables names or node ids.
    //           (date-entity :day 5 :month 1 :year 2002)

    // Detail 1: Variable names included.
    //           (d / date-entity :day 5 :month 1 :year 2002)

    // Detail 2: Nodes are labelled with id.
    //           ([0] d / date-entity :day [0.2] 5 :month [0.1] 1 :year [0.0] 2002)

    // Boolean 'pretty' indicates whether to indent into pretty format or leave on one line

        prettyString(detail, pretty, "")
    }

    private def prettyString(detail: Int, pretty: Boolean, indent: String) : String = {
        var nextIndent = ""
        var prefix = "" 
        if (pretty) {   // prefix for the children (so that it goes ':ARG0 concept' on the same line)
            nextIndent = indent + "      "  // indent by six
            prefix = "\n" + nextIndent
        }
        if (name != None) {
            val Some(n) = name
            if (relations.size != 0) {      // Concept with name and children
                detail match {
                    case 0 =>
                "("+concept+" "+(topologicalOrdering.map(x => prefix+x._1+" "+x._2.prettyString(detail, pretty, nextIndent)) :::
                                 variableRelations.map(x => prefix+x._1+" "+x._2.node.concept)).sorted.mkString(" ")+")"
                    case 1 =>
                "("+n+" / "+concept+" "+(topologicalOrdering.map(x => prefix+x._1+" "+x._2.prettyString(detail, pretty, nextIndent)) :::
                                 variableRelations.map(x => prefix+x._1+" "+x._2.name)).sorted.mkString(" ")+")"
                    case 2 =>
                "(["+id+"] "+n+" / "+concept+" "+(topologicalOrdering.map(x => prefix+x._1+" "+x._2.prettyString(detail, pretty, nextIndent)) :::
                                 variableRelations.map(x => prefix+x._1+" ["+x._2.node.id+"] "+x._2.name)).sorted.mkString(" ")+")"
                }
            } else {                        // Concept with name, but no children
                detail match {
                    case 0 =>
                        concept
                    case 1 =>
                        "("+n+" / "+concept+")"
                    case 2 =>
                        "(["+id+"] "+n+" / "+concept+")"
                }
            }
        } else if (relations.size == 0) {   // Concept with no name and no children
            if (detail < 2) {
                concept
            } else {
                "["+id+"] "+concept
            }
        } else {                            // Concept with no name but has children
            if (detail == 0) {
                "("+concept+" "+(topologicalOrdering.map(x => prefix+x._1+" "+x._2.prettyString(detail, pretty, nextIndent)) :::
                                 variableRelations.map(x => prefix+x._1+" "+x._2.node.concept)).sorted.mkString(" ")+")"
            } else if (detail == 1) {
                "("+concept+" "+(topologicalOrdering.map(x => prefix+x._1+" "+x._2.prettyString(detail, pretty, nextIndent)) :::
                                 variableRelations.map(x => prefix+x._1+" "+x._2.name)).sorted.mkString(" ")+")"
            } else {
                "(["+id+"] "+concept+" "+(topologicalOrdering.map(x => prefix+x._1+" "+x._2.prettyString(detail, pretty, nextIndent)) :::
                            variableRelations.map(x => prefix+x._1+" ["+x._2.node.id+"] "+x._2.name)).sorted.mkString(" ")+")"
            }
        }
    }
}

case class Graph(root: Node, spans: ArrayBuffer[Span], getNodeById: Map[String, Node], getNodeByName: Map[String, Node]) {
    def loadSpans(spanStr: String, sentence: Array[String]) = {
        assert(spans.size == 0, "This code does not support loading new spans")
        //spans.clear
        val SpanRegex = """([*]?)([0-9]+)-([0-9]+)\|(.*)""".r
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
                // TODO: catch malformed input (Regex match error, or toInt err
                case e => logger(1, "****************** MALFORMED SPAN: "+spanStr)
            }
        }
    }


    def addSpan(start: Int, end: Int, nodeIds: List[String], coRef: Boolean, sentence: Array[String]) {
        val span = Span(start, end, nodeIds, sentence.slice(start, end).mkString(" "), SpanLoader.getAmr(nodeIds, this), coRef)
        spans += span
        for (id <- nodeIds) {
            getNodeById(id).addSpan(spans.size-1, coRef)
        }
    }

    def addSpan(span: Span) {
        spans += span
        for (id <- span.nodeIds) {
            getNodeById(id).addSpan(spans.size-1, span.coRef)
        }
    }

    def overlap(span: Span) : Boolean = {
        var overlap = false
        for (id <- span.nodeIds) {
            overlap = (getNodeById(id).spans.map(x => !spans(x).coRef) :\ overlap)(_ || _)
        }
        return overlap
    }

    def updateSpan(spanIndex: Int, start: Int, end: Int, nodeIds: List[String], coRef: Boolean, sentence: Array[String]) {
        println("new nodes = "+nodeIds.toString)
        for (id <- spans(spanIndex).nodeIds) {
            println(getNodeById(id).spans)
            getNodeById(id).spans -= spanIndex
        }
        val span = Span(start, end, nodeIds, sentence.slice(start, end).mkString(" "), SpanLoader.getAmr(nodeIds, this), coRef)
        spans(spanIndex) = span
        for (id <- nodeIds) {
            getNodeById(id).addSpan(spanIndex, coRef)
            println(getNodeById(id).spans)
        }
    }

    def updateSpan(spanIndex: Int, coRef: Boolean, sentence: Array[String]) {
        val span = spans(spanIndex)
        updateSpan(spanIndex, span.start, span.end, span.nodeIds, coRef, sentence)
    }

    def addAllSpans(f: AlignSpans2.SpanAligner) {
        def add(node: Node) {
            for (span <- f.getSpans(node)) {
                if(span.coRef || !overlap(span)) {
                    addSpan(span)
                }
            }
            //f(node).map(addSpan(_))
        }
        doRecursive(root, add)
    }

    def doRecursive(node: Node, f: (Node) => Unit) {
        f(node)
        for ((_,child) <- node.topologicalOrdering) {
            doRecursive(child, f)
        }
    }

/* Very cool recursive function TODO: use this one instead
    def doRecursive(parentMessage: T, node: Node, f: (Node, T) => T, g: List[R] => R ) : R = {
        val message = f(parentMessage, node)
        return g(node.topologicalOrdering.map(x => doRecursive(message, x.2, f, g)).toList)
    }
*/

    def makeIds() {
        // Sets the node.id field for every node in the graph according to the topologicalOrdering
        // For example "0" is the root and "0.1" is the 2nd child of the root
        // Assumes that a topological ordering already exists (node.topologicalOrdering is non-empty)
        getNodeById.clear
        makeIds(root, List(0))
    }

    private def makeIds(node: Node, id: List[Int]) {
        node.id = id.mkString(".")
        getNodeById += (node.id -> node)
        for (((_,child), i) <- node.topologicalOrdering.zipWithIndex) {
            makeIds(child, id ::: List(i))
        }
    }

    private def makeVariables() {
        // Populate the getNodeByName map
        // Assumes a topologicalOrdering exists and node.name is set
        getNodeByName.clear
        makeVariables(root)
    }

    private def makeVariables(node: Node) {
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

    private def unifyVariables() {
        // Postcondition: Unify variables, remove variables from node.topologicalOrdering,
        // and populate node.relations and node.variableRealtions attributes for each node
        // Precondition: topologicalOrdering was filled in by the graph parser
        // and that makeVariables has already been called
        unifyVariables(root)
    }

    private def unifyVariables(node: Node) {
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
        graph.makeVariables
        graph.unifyVariables
        graph.makeIds
        return graph
    }
}

