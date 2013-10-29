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
import java.util.Date
import java.text.SimpleDateFormat
import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator._

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

        for { block <- Corpus.splitOnNewline(Source.stdin.getLines)
              if (block matches "(.|\n)*\n\\((.|\n)*") } {
            println("**** Processsing Block *****")
            println(block)
            println("****************************")
            val Block(extrastr, amrstr) = block
            val graph = Graph.parse(amrstr)
            logger(1,graph.root.prettyString(detail = 2, pretty = true))
            graph.makeTopologicalOrdering
            println(graph.root.prettyString(detail = 1, pretty = true))
        }

    }

}


