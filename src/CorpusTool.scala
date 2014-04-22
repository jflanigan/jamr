package edu.cmu.lti.nlp.amr


import scala.collection.mutable.Map

import Corpus._

object CorpusTool {
    val usage = """Usage: scala -classpath . edu.cmu.lti.nlp.amr.CorpusTool < amr_corpus --tokenized tokenized_sentences > new_amr_corpus"""
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

        if (args.length == 0) { println(usage); sys.exit(1) }

        val options = parseOptions(Map(),args.toList)
        if (options.contains('verbosity)) {
            verbosity = options('verbosity).asInstanceOf[Int]
        }
        if (!options.contains('tokenized)) {
            System.err.println("Error: No tokenized file specified")
            sys.exit(1)
        }

        val tokenized = Source.fromFile(options('tokenized).asInstanceOf[String]).getLines.toArray

        val Block = """((?:\n|.)*)\n(\((?:\n|.)*)""".r  // (?: ) is non-capturing group
        var i = 0
        for (block <- splitOnNewline(Source.stdin.getLines)) {
            if (block matches "(.|\n)*\n\\((.|\n)*") { // . does not match \n
                val Block(extras, amr) = block
                println(extras)
                println("# ::tok " + tokenized(i))
                println(amr+"\n")
                i += 1
            } else {
                println(block+"\n")
            }
        }
    }
}

