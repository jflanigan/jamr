package edu.cmu.lti.nlp.amr

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

/****************************** Driver Program *****************************/
object TestAMRDisplayCode {

    val usage = """Usage: scala -classpath . edu.cmu.lti.nlp.amr.TestAMRDisplayCode < amr_file > outfile"""
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

        val Block = """((?:\n|.)*)\n(\((?:\n|.)*)""".r  // (?: ) is non-capturing group
                                                        // and . does not match \n

        for { block <- Corpus.getAMRBlocks(Source.stdin.getLines) } {
            logger(0,"**** Processsing Block *****")
            logger(0,block)
            logger(0,"****************************")
            val Block(extrastr, amrstr) = block
            val graph = Graph.parse(amrstr)
            logger(1,graph.prettyString(detail = 2, pretty = true))
            graph.normalizeInverseRelations
            graph.makeTopologicalOrdering
            println(graph.prettyString(detail = 1, pretty = true))
            println()
        }

    }

}


