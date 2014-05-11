package edu.cmu.lti.nlp.amr.Generate
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

object ExtractPhrases2 {

    val usage = """Usage: scala -classpath . edu.cmu.lti.nlp.amr.Generate.ExtractPhrases < amr_file > outfile"""
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
            val sentence = data.sentence    // Tokenized sentence
            val spans = graph.spans.sortBy(x => x.start)
            val getSpan : Array[Option[Span]] = sentence.map(x => None).toArray
            for { span <- spans
                  i <- span.start until span.end } {
                getSpan(i) = Some(span)
            }
            for (span <- spans) {
                var start = span.start
                do {
                    var end = span.end
                    do {
                        var amr = PrintTrees.printRecursive(span.amr)
                        if (!amr.startsWith("(")) {
                            amr = "(C "+amr+")"
                        }
                        println(amr  + " ||| " + sentence.slice(start,end).mkString(" "))
                        end += 1
                    } while (end < sentence.size && getSpan(end) == None)
                    start -= 1
                } while (start >= 0 && getSpan(start) == None)
            }
        }
    }
}

