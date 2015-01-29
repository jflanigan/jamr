package edu.cmu.lti.nlp.amr

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}


object TestAMRCode {

    val usage = """Usage: scala -classpath . edu.cmu.lti.nlp.amr.TestAMRCode"""
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

    def main(args: Array[String]) {
        val options = parseOptions(Map(),args.toList)
        if (options.contains('help)) { println(usage); sys.exit(1) }

        if (options.contains('verbosity)) {
            verbosity = options('verbosity).asInstanceOf[Int]
        }

        for (block <- Corpus.getAMRBlocks(io.Source.stdin.getLines())) {
            println(block)
            AMRTrainingData(block).toOracleGraph(clearUnalignedNodes = false).printTriples(0)
            println()
        }
    }
}

