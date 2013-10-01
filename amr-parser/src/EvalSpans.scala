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

object EvalSpans {

    val usage = """Usage: scala -classpath . edu.cmu.lti.nlp.amr.EvalSpans < amr_file"""
    type OptionMap = Map[Symbol, Any]

    def parseOptions(map : OptionMap, list: List[String]) : OptionMap = {
        def isSwitch(s : String) = (s(0) == '-')
        list match {
            case Nil => map
            //case "--train" :: tail =>
            //          parseOptions(map ++ Map('train -> true), tail)
            //case "-a" :: value :: tail =>
            //          parseOptions(map ++ Map('amrfile -> value), tail)
            //case "--only" :: tail =>
            //          parseOptions(map ++ Map('only -> true), tail)
            case "-h" :: value :: tail =>
                      parseOptions(map ++ Map('help -> value.toInt), tail)
            case "-v" :: value :: tail =>
                      parseOptions(map ++ Map('verbosity -> value.toInt), tail)
             //case string :: opt2 :: tail if isSwitch(opt2) => 
            //          parseOptions(map ++ Map('infile -> string), list.tail)
            //case string :: Nil =>  parseOptions(map ++ Map('infile -> string), list.tail)
            case option :: tail => println("Error: Unknown option "+option) 
                               sys.exit(1) 
      }
    }

    def main(args: Array[String]) {
        val options = parseOptions(Map(),args.toList)
        if (options.contains('help)) { println(usage); sys.exit(1) }

        verbosity = 0
        if (options.contains('verbosity)) {
            verbosity = options('verbosity).asInstanceOf[Int]
        }

        var correct = 0.0
        var aligner_total = 0.0
        var gold_total = 0.0

        val Alignments = """.*::alignments ([^:]*) .*""".r

        for (block <- Corpus.splitOnNewline(Source.stdin.getLines)) {
            val lines = block.split("\n")
            val alignerStrs = lines.filter(x => x.matches(".*Aligner .*"))
            val annotatorStrs = lines.filter(x => x.matches(".*AlignerTool.*")).filterNot(x => x.matches(".*Aligner .*"))
            if (alignerStrs.size != 0 && annotatorStrs.size != 0) {
                val Alignments(alignerStr) = alignerStrs(alignerStrs.size-1)
                val Alignments(annotatorStr) = annotatorStrs(annotatorStrs.size-1)
                val aligner = alignerStr.split(" ").filterNot(_.matches(""))
                val annotator = annotatorStr.split(" ").filterNot(_.matches(""))
                logger(2,"aligner = "+aligner.toList.toString)
                logger(2,"annotator = "+annotator.toList.toString)
                aligner_total += aligner.size
                gold_total += annotator.size
                logger(2,"diff = "+annotator.distinct.diff(annotator.diff(aligner)).toList.toString)
                correct += annotator.distinct.diff(annotator.diff(aligner)).size
            }
            
        }

        logger(1,"correct = "+correct.toString)
        logger(1,"aligner_total = "+aligner_total.toString)
        logger(1,"gold_total = "+gold_total.toString)

        val p = correct/aligner_total
        val r = correct/gold_total
        val f1 = 2.0*p*r/(p+r)

        println("Precision = "+p.toString)
        println("Recall = "+r.toString)
        println("F1 = "+f1.toString)
    }
}


