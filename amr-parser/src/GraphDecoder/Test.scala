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

    def main(args: Array[String]) {
        val options = parseOptions(Map(),args.toList)
        if (options.contains('help)) { println(usage); sys.exit(1) }

        if (options.contains('verbosity)) {
            verbosity = options('verbosity).asInstanceOf[Int]
        }

        var aligner2 = true
        if (options.contains('aligner1)) {
            aligner2 = false
        }

        val sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS")
        val Block = """((?:\n|.)*)\n(\((?:\n|.)*)""".r  // (?: ) is non-capturing group
                                                        // and . does not match \n
        for (block <- Corpus.splitOnNewline(Source.stdin.getLines)) {
            if (block matches "(.|\n)*\n\\((.|\n)*") {  // Does it contain some AMR?
                logger(2,"**** Processsing Block *****")
                logger(2,block)
                logger(2,"****************************")
                val Block(extrastr, amrstr) = block
                println(extrastr)
                val amr = Graph.parse(amrstr)
                val extras = Corpus.getUlfString(extrastr)
                val tokenized = extras("::tok").split(" ")
                val wordAlignments = AlignWords.alignWords(tokenized, amr)
                val spanAlignments = if (aligner2) {
                        AlignSpans2.align(tokenized, amr)
                    } else {
                        AlignSpans.alignSpans(tokenized, amr, wordAlignments)
                    }
                AlignSpans.logUnalignedConcepts(amr.root)

                val spans = amr.spans
                for ((span, i) <- spans.zipWithIndex) {
                    logger(1, "Span "+(i+1).toString+":  "+span.words+" => "+span.amr)
                    logger(3, "* "+span.format)
                }
                println("# ::alignments "+spans.map(_.format).mkString(" ")+" ::annotator Aligner v.02 ::date "+sdf.format(new Date))
                println(amrstr+"\n")
            } else {
                println(block+"\n")
            }
        }
    }

}

