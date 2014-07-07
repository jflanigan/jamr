package edu.cmu.lti.nlp.amr

import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

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

        var i = 0
        for (block <- splitOnNewline(Source.stdin.getLines)) {
            if (block.split("\n").exists(_.startsWith("("))) {  // needs to contain come AMR
                val extras : String = block.split("\n[(]")(0)
                val amr : String = block.split("\n[(]").tail.mkString("\n(")
                println(extras)
                println("# ::tok " + tokenized(i))
                println("("+amr+"\n")
                i += 1
            } else {
                println(block+"\n")
            }
        }
    }
}

