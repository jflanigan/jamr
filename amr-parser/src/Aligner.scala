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
import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator._

/****************************** Driver Program *****************************/
object Aligner {

    val usage = """Usage: scala -classpath . edu.cmu.lti.nlp.amr.Aligner -a amr_file -e english_file > alignments"""
    type OptionMap = Map[Symbol, Any]

    def parseOptions(map : OptionMap, list: List[String]) : OptionMap = {
        def isSwitch(s : String) = (s(0) == '-')
        list match {
            case Nil => map
            //case "--train" :: tail =>
            //          parseOptions(map ++ Map('train -> true), tail)
            case "-a" :: value :: tail =>
                      parseOptions(map ++ Map('amrfile -> value), tail)
            case "-e" :: value :: tail =>
                      parseOptions(map ++ Map('englishfile -> value), tail)
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

        if (args.length == 0) { println(usage); sys.exit(1) }

        val options = parseOptions(Map(),args.toList)
        if (options.contains('verbosity)) {
            verbosity = options('verbosity).asInstanceOf[Int]
        }
        if (!options.contains('englishfile)) {
            System.err.println("Error: No English source file specified")
            sys.exit(1)
        }
        if (!options.contains('amrfile)) {
            System.err.println("Error: No AMR file specified")
            sys.exit(1)
        }

        val sentences = Source.fromFile(options('englishfile).asInstanceOf[String]).getLines
        val amrs = Source.fromFile(options('amrfile).asInstanceOf[String]).getLines

        for ((sentence, amrstr) <- sentences.zip(amrs)) {
            println(sentence)
            println(amrstr)
            val amr = Graph.parse(amrstr)
            //println(amr)
            val tokenized = sentence.split(" ")
            //println(tokenized.toList)
            val wordAlignments = AlignWords.alignWords(tokenized, amr)
            val spanAlignments = AlignSpans.alignSpans(tokenized, amr, wordAlignments)
            AlignSpans.logUnalignedConcepts(amr.root)
/*            for ((word, node) <- tokenized.zip(wordAlignments)) {
                node match {
                    case Some(n) => { n.name match {
                        case Some(name) =>  print(word+":("+name+" / "+n.concept+") ")
                        case _ => print(word+":"+n.concept+" ") } }
                    case _ => print(word+" ")
                }
            }
            println() */
            val spans = amr.spans
            //println(spanAlignments.toList)
            //println(spans)
/*            for ((word, spanIndex) <- tokenized.zip(spanAlignments)) {
                spanIndex match {
                    case Some(i) => { spans(i).amr.name match {
                        case Some(name) =>  print(word+":("+name+" / "+spans(i).amr.concept+") ")
                        case _ => print(word+":"+spans(i).amr.concept+" ") } }
                    case _ => print(word+" ")
                }
            }
            println() */
            for ((span, i) <- spans.zipWithIndex) {
                println("Span "+(i+1).toString+":  "+span.words+" => "+span.amr)
            }
            println()
        }
    }

}

