package edu.cmu.lti.nlp.amr.GraphDecoder
import edu.cmu.lti.nlp.amr._

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
import Double.{NegativeInfinity => minusInfty}


object Test {

    val usage = """Usage: scala -classpath . edu.cmu.lti.nlp.amr.GraphDecoder.Test"""
    type OptionMap = Map[Symbol, Any]

    def parseOptions(map : OptionMap, list: List[String]) : OptionMap = {
        def isSwitch(s : String) = (s(0) == '-')
        list match {
            case Nil => map
            case "-h" :: tail =>
                      parseOptions(map ++ Map('help -> true), tail)
            case "-v" :: value :: tail =>
                      parseOptions(map ++ Map('verbosity -> value.toInt), tail)
            case option :: tail => println("Error: Unknown option "+option) 
                               sys.exit(1) 
      }
    }

    def node(name: String) : Node = {
        return Node(name, Some(name), name, List(), List(), List(), None, ArrayBuffer())
    }

    def weights(weightList : List[(String, String, String, Double)]) : FeatureVector = {
        return FeatureVector(
            Map() ++ (for ((node1, node2, relation, weight) <- weightList) yield {
                ("Id1="+node1+":Id2="+node2+":L="+relation, weight)
            }).toMap)
    }

    def main(args: Array[String]) {
        val options = parseOptions(Map(),args.toList)
        if (options.contains('help)) { println(usage); sys.exit(1) }

        if (options.contains('verbosity)) {
            verbosity = options('verbosity).asInstanceOf[Int]
        }

        test1()
        test2()
    }

    def test1() {
        println("Test1")
        val nodes = Map("1" -> node("1"),
                        "2" -> node("2"),
                        "3" -> node("3"))
        val graph = Graph(nodes("1"), ArrayBuffer(), nodes, nodes)
        val decoder = new Alg1(List("edgeId"), Array((":r", 1)))
        decoder.features.weights = weights(
            List(("1", "2", ":r", 6),
                 ("2", "3", ":r", -6),
                 ("1", "3", ":r", 3)))
        val result = decoder.decode(Input(graph, Array(), Array()))
        result.graph.printTriples(detail = 1)
    }

    def test2() {
        println("Test2")
        val nodes = Map("1" -> node("1"),
                        "2" -> node("2"),
                        "3" -> node("3"))
        val graph = Graph(nodes("1"), ArrayBuffer(), nodes, nodes)
        val decoder = new Alg2(List("edgeId"), Array((":r", 1)))
        decoder.features.weights = weights(
            List(("1", "2", ":r", 6),
                 ("2", "3", ":r", -6),
                 ("1", "3", ":r", -3)))
        val result = decoder.decode(Input(graph, Array(), Array()))
        result.graph.printTriples(detail = 1)
    }
}

