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
/*  TODO: resurrect this code

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

//        test1()
        test2()
//        test3()
//        test4()
//        samTest()
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
        val result = decoder.decode(Input(graph, Array(), Annotation(Array(), Array(), Array()), Annotation(Array(), Array(), Array())))
        graph.printTriples(detail = 1)
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
                 ("1", "3", ":r", 3)))
        println("weights:")
        print(decoder.features.weights)
        val result = decoder.decode(Input(graph, Array(), Annotation(Array(), Array(), Array()), Annotation(Array(), Array(), Array())))
        println("In test2()")
        for (node <- result.graph.nodes) { println(node.topologicalOrdering.map(x => (x._1, x._2.id))) }
        println("Triples:")
        result.graph.printTriples(detail = 1)
        println("TopologicalOrdering:")
        for (node <- result.graph.nodes) {
            println(node.id + "=" + node.topologicalOrdering.map(x => (x._1, x._2.id)))
        }
        println("Graph:")
        println(result.graph.root.prettyString(detail = 1, pretty = true))
    }

    def test3() {
        println("Test3")
        val nodes = Map("1" -> node("1"),
                        "2" -> node("2"),
                        "3" -> node("3"))
        val graph = Graph(nodes("1"), ArrayBuffer(), nodes, nodes)
        val decoder = new DualDecomposition(List("edgeId"), Array((":r", 1)), 1)
        decoder.features.weights = weights(
            List(("1", "2", ":r", 6),
                 ("2", "3", ":r", -6), // 5
                 ("1", "3", ":r", 3))) // 1
        val result = decoder.decode(Input(graph, Array(), Annotation(Array(), Array(), Array()), Annotation(Array(), Array(), Array())))
        result.graph.printTriples(detail = 1)
    }

    def test4() {
        println("Test4")
        val nodes = Map("1" -> node("1"),
                        "2" -> node("2"),
                        "3" -> node("3"),
                        "4" -> node("4"))
        nodes("3").relations = List((":r", nodes("4")))
        val graph = Graph(nodes("1"), ArrayBuffer(), nodes, nodes)
        val decoder = new DualDecomposition(List("edgeId"), Array((":r", 1)), 1)
//        val decoder = new Alg2(List("edgeId"), Array((":r", 1)))
        decoder.features.weights = weights(
            List(("1", "2", ":r", 6),
                 ("2", "3", ":r", -6), // 5
                 ("1", "3", ":r", 3))) // 1
        val result = decoder.decode(Input(graph, Array(), Annotation(Array(), Array(), Array()), Annotation(Array(), Array(), Array())))
        result.graph.printTriples(detail = 1)
    }

    def test5() {
        println("Test5")
        val nodes = Map("1" -> node("1"),
                        "2" -> node("2"),
                        "3" -> node("3"),
                        "4" -> node("4"))
        nodes("3").relations = List((":r", nodes("4")))
        val graph = Graph(nodes("1"), ArrayBuffer(), nodes, nodes)
        val decoder = new Alg2(List("edgeId"), Array((":r", 1)))
        decoder.features.weights = weights(
            List(("1", "2", ":r", 6),
                 ("2", "3", ":r", -6), // 5
                 ("1", "3", ":r", 0))) // 1
        val result = decoder.decode(Input(graph, Array(), Annotation(Array(), Array(), Array()), Annotation(Array(), Array(), Array())))
        result.graph.printTriples(detail = 1)
    }

    def samTest() {
        println("samTest")
        val nodes = Map("1" -> node("1"),
                        "2" -> node("2"),
                        "3" -> node("3"),
                        "4" -> node("4"))
        val graph = Graph(nodes("1"), ArrayBuffer(), nodes, nodes)
        val decoder = new DualDecomposition(List("edgeId"), Array((":r", 1), (":s", 1)), 1)
        decoder.features.weights = weights(
            List(("1", "2", ":s", -100),
                 ("1", "3", ":r", 1),
                 ("4", "2", ":r", 1),
                 ("4", "3", ":r", -1)))
        for { node1 <- nodes.keys
              node2 <- nodes.keys
              label <- List(":r", ":s")
              if (!decoder.features.weights.fmap.contains("Id1="+node1+":Id2="+node2+":L="+label)) } {
            decoder.features.weights.fmap("Id1="+node1+":Id2="+node2+":L="+label) = minusInfty
        }
        val result = decoder.decode(Input(graph, Array(), Annotation(Array(), Array(), Array()), Annotation(Array(), Array(), Array())))
        result.graph.printTriples(detail = 1)
    }
}

*/
