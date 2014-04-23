package edu.cmu.lti.nlp.amr.Generate
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

object PrintTrees {

    val usage = """Usage: scala -classpath . edu.cmu.lti.nlp.amr.Generate.PrintTrees < amr_file > outfile"""
    type OptionMap = Map[Symbol, Any]

    def parseOptions(map : OptionMap, list: List[String]) : OptionMap = {
        def isSwitch(s : String) = (s(0) == '-')
        list match {
            case Nil => map
            case "-h" :: value :: tail =>
                      parseOptions(map ++ Map('help -> value.toInt), tail)
            case "-v" :: value :: tail =>
                      parseOptions(map ++ Map('verbosity -> value.toInt), tail)
            case option :: tail => println("Error: Unknown option "+option) 
                               sys.exit(1) 
      }
    }

    def main(args: Array[String]) {
        val options = parseOptions(Map(),args.toList)
        if (options.contains('help)) { println(usage); sys.exit(1) }

        if (options.contains('verbosity)) {
            verbosity = options('verbosity).asInstanceOf[Int]
        }

        for { block <- Corpus.splitOnNewline(Source.stdin.getLines)
              if (block matches "(.|\n)*\n\\((.|\n)*") } {
            logger(1,"**** Processsing Block *****")
            logger(1,block)
            logger(1,"****************************")
            val data = AMRTrainingData(block)
            val graph = data.toOracleGraph(clearUnalignedNodes = false)

            println(printRecursive(graph.root))
        }
    }

    def printRecursive(node: Node) : String = {
        val concept = node.concept.replaceAll("""\(""", "-LBR-").replaceAll("""\)""", "-RBR-")
        if (node.children.size == 0) {
            if (node.concept.startsWith("\"")) {
                "(S "+concept.slice(1,concept.size-1)+")"
            } else {
                concept
            }
        } else {
            // Example: (X (X hit-01) (ARG0 ___) (ARG1 ___))
            val list = node.children.sortBy(_._1).map(x => "("+x._1.drop(1).toUpperCase.replaceAll("-","_")+" "+printRecursive(x._2)+")")
            "(X (X "+concept+") "+list.mkString(" ")+")"
        }
    }
}

