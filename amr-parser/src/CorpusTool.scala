package edu.cmu.lti.nlp.amr

import scala.swing._
import scala.swing.event._

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

        val tokenized = Source.fromFile(options('tokenized).asInstanceOf[String])

        val Block = """((?:\n|.)*)\n(\((?:\n|.)*)""".r  // (?: ) is non-capturing group
        for ((block, tokens) <- (splitOnNewline(Source.stdin.getLines) zip tokenized.getLines)) {
            if (block matches "(.|\n)*\n\\((.|\n)*") { // . does not match \n
                val Block(extras, amr) = block
                println(extras)
                println("# ::tok " + tokens)
                println(amr+"\n")
            } else {
                println(block+"\n")
            }
        }
    }
}

