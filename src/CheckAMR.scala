package edu.cmu.lti.nlp.amr

import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

object CheckAMR {
    val usage = """Usage: scala -classpath . edu.cmu.lti.nlp.amr.CheckAMR < amr_corpus > output"""
    type OptionMap = Map[Symbol, Any]

    def parseOptions(map : OptionMap, list: List[String]) : OptionMap = {
        def isSwitch(s : String) = (s(0) == '-')
        list match {
            case Nil => map
            case "--tokenized" :: value :: tail =>
                      parseOptions(map ++ Map('tokenized -> value), tail)
            case "-v" :: value :: tail =>
                      parseOptions(map ++ Map('verbosity -> value.toInt), tail)
            case option :: tail => println("Error: Unknown option "+option)
                               sys.exit(1)
      }
    }

    def main(args: Array[String]) {

        val options = parseOptions(Map(),args.toList)
        if (options.contains('verbosity)) {
            verbosity = options('verbosity).asInstanceOf[Int]
        }

        val Block = """((?:\n|.)*)\n(\((?:\n|.)*)""".r  // (?: ) is non-capturing group
        var i = 0
        for (block <- Corpus.splitOnNewline(Source.stdin.getLines)) {
            //if (block matches "(.|\n)*\n\\((.|\n)*") { // Does it containt some AMR? . does not match \n
                //val Block(extras, amrstr) = block
                val graph = Graph.parse(block)
                //graph.normalizeInverseRelations
                if (CycleTester.hasCycle(graph.nodes.toList, graph.nodes.map(x => (x, x.relations.map(y => y._2))).toMap)) {
                    println("Contains a cycle")
                } else {
                    println("No cycle")
                }
                var simple = true
                for (node1 <- graph.nodes) {
                    for ((_,node2) <- node1.relations) {
                        var count = 0
                        for ((_,node3) <- node1.relations) {
                            if (node3.id == node2.id) {
                                count += 1
                            }
                        }
                        for ((_,node3) <- node2.relations) {
                            if (node3.id == node1.id) {
                                count += 1
                            }
                        }
                        if (count > 1) {
                            simple = false
                        }
                    }
                }
                if (simple) {
                    println("Graph simple")
                } else {
                    println("Graph not simple")
                }
                i += 1
            //} else {
            //    println(block+"\n")
            //}
        }
    }
}

