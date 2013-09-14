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

case class Node(name: Option[String], concept: String, var relations: List[(String, Node)], var alignment: Option[Int], var span: Option[Int]) {
    override def toString() : String = {
        if (name != None) {
            val Some(n) = name
            if (relations.size != 0) {
                //"("+n+" / "+concept+" "+relations.map(x => x._1+" "+x._2.toString).mkString(" ")+")"
                "("+concept+" "+relations.map(x => x._1+" "+x._2.toString).mkString(" ")+")"
            } else {
                //"("+n+" / "+concept+")"
                "("+concept+")"
            }
        } else if (relations.size == 0) {
            concept
        } else {
            "("+concept+" "+relations.map(x => x._1+" "+x._2.toString).mkString(" ")+")"
        }
    }
}

case class Graph(root: Node, spans: ArrayBuffer[Span])

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
            case variable~"/"~concept~relations => Node(Some(variable), concept, relations, None, None)
        }
        def terminalNode : Parser[Node] = concept ^^ { 
            case concept => Node(None, concept, List[(String, Node)](), None, None)
        }
        def node : Parser[Node] = terminalNode | internalNode
    }

    private val parser = new GraphParser()

    def load(iterator: Iterator[String]) : Iterator[Graph] = {
        for (line <- iterator) yield {
            parser.parseAll(parser.node, line) match {
                case parser.Success(e, _) => Graph(e, new ArrayBuffer[Span]())
            }
        }
    }

    def parse(amr: String) : Graph = {
        parser.parseAll(parser.node, amr) match {
            case parser.Success(e, _) => Graph(e, new ArrayBuffer[Span]())
        }
    }
}

