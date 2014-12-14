package edu.cmu.lti.nlp.amr

import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

import Corpus._

// TODO: this class can be removed

object PrintSpans {
    val usage = """Usage: scala -classpath . edu.cmu.lti.nlp.amr.PrintSpans < amr_corpus > span_file"""
    type OptionMap = Map[Symbol, Any]

    def parseOptions(map : OptionMap, list: List[String]) : OptionMap = {
        def isSwitch(s : String) = (s(0) == '-')
        list match {
            case Nil => map
            case "-v" :: value :: tail =>
                      parseOptions(map ++ Map('verbosity -> value.toInt), tail)
            case option :: tail => println("Error: Unknown option "+option)
                               sys.exit(1)
      }
    }

    def main(args: Array[String]) {

        if (args.length != 0) { println(usage); sys.exit(1) }

        val options = parseOptions(Map(),args.toList)
        if (options.contains('verbosity)) {
            verbosity = options('verbosity).asInstanceOf[Int]
        }

        var i = 0
        for { b <- splitOnNewline(Source.stdin.getLines)
              if (b.split("\n").exists(_.startsWith("("))) } {  // needs to contain come AMR
            val block = AMRTrainingData(b)
            println(b)
            block.loadSpans()
            for ((span, i) <- block.graph.spans.zipWithIndex) {
                logger(1, "Span "+(i+1).toString+":  "+span.words+" => "+span.amr)
                logger(3, "* "+span.format)
            }
            println()
        }
    }
}

