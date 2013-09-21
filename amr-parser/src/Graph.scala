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

case class Node(var id: String, name: Option[String], concept: String, var relations: List[(String, Node)], var topologicalOrdering: List[(String, Node)], var variableRelations: List[(String, Var)], var alignment: Option[Int], var span: Option[Int]) {
    override def toString() : String = {
        if (name != None) {  // TODO: fix this up
            val Some(n) = name
            if (relations.size != 0) {
                //"("+n+" / "+concept+" "+relations.map(x => x._1+" "+x._2.toString).mkString(" ")+")"
                //"("+concept+" "+relations.map(x => x._1+" "+x._2.toString).mkString(" ")+")"
                "("+concept+" "+topologicalOrdering.map(x => x._1+" "+x._2.toString).mkString(" ")+" "+
                                variableRelations.map(x => x._1+" "+x._2.name).mkString(" ")+")"
            } else {
                //"("+n+" / "+concept+")"
                "("+concept+")"
            }
        } else if (relations.size == 0) {
            concept
        } else {
            "("+concept+" "+topologicalOrdering.map(x => x._1+" "+x._2.toString).mkString(" ")+" "+
                            variableRelations.map(x => x._1+" "+x._2.name).mkString(" ")+")"
        }
    }

    def fullString() : String = {
        if (name != None) {  // TODO: fix this up
            val Some(n) = name
            if (relations.size != 0) {
                //"("+n+" / "+concept+" "+relations.map(x => x._1+" "+x._2.toString).mkString(" ")+")"
                //"("+concept+" "+relations.map(x => x._1+" "+x._2.toString).mkString(" ")+")"
                "(["+id+"] "+n+" / "+concept+" "+topologicalOrdering.map(x => x._1+" "+x._2.fullString).mkString(" ")+" "+
                                        variableRelations.map(x => x._1+" ["+x._2.node.id+"] "+x._2.name).mkString(" ")+")"
            } else {
                "(["+id+"] "+n+" / "+concept+")"
                //"("+concept+")"
            }
        } else if (relations.size == 0) {
            "["+id+"] "+concept
        } else {
            "(["+id+"] "+concept+" "+topologicalOrdering.map(x => x._1+" "+x._2.fullString).mkString(" ")+" "+
                            variableRelations.map(x => x._1+" ["+x._2.node.id+"] "+x._2.name).mkString(" ")+")"
        }
    }
}

case class Graph(root: Node, spans: ArrayBuffer[Span], getNodeByName: Map[String, Node]) {
    def getNodeById(idStr : String) : Node = {
        // Get node according to string id (for example "0" is the root and "0.1" is the 2nd child of the root)
        try {
            val id = idStr.split("[.]").map(_.toInt).toList // split takes a regex string
            return getNodeById(root, id.tail)
        } catch {
            case _ => throw new RuntimeException("can't find node by id = " + idStr)
        }
    }

    private def getNodeById(node : Node, id : List[Int]) : Node = {
        if (id.size == 0) {
            node
        } else {
            getNodeById(node.relations(id(0))._2, id.tail)
        }
    }

    private def makeIds() {
        // Sets the node.id field for every node in the graph
        // Assumes that a topological ordering already exists (node.topologicalOrdering is non-empty)
        makeIds(root, List(0))
    }

    private def makeIds(node: Node, id: List[Int]) {
        node.id = id.mkString(".")
        for (((_,child), i) <- node.topologicalOrdering.zipWithIndex) {
            makeIds(child, id ::: List(i))
        }
    }

    private def makeVariables() {
        // Populate the getNodeByName map
        // Assumes topologicalOrdering exists and node.name is set
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
        // Unify variables, remove variables from node.topologicalOrdering,
        // and populate node.relations and node.variableRealtions attributes for each node
        // Assumes that topologicalOrdering was filled in by the graph parser
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
            case variable~"/"~concept~relations => Node("", Some(variable), concept, List[(String, Node)](), relations, List[(String, Var)](), None, None)
        }
        def terminalNode : Parser[Node] = concept ^^ { 
            case concept => Node("", None, concept, List[(String, Node)](), List[(String, Node)](), List[(String, Var)](), None, None)
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
            case parser.Success(e, _) => Graph(e, new ArrayBuffer[Span](), Map[String, Node]())
        }
        graph.makeVariables
        graph.unifyVariables
        graph.makeIds
        return graph
    }
}

