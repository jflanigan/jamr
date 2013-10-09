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
object Aligner {

    val usage = """Usage: scala -classpath . edu.cmu.lti.nlp.amr.Aligner < amr_file > alignments"""
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
            case "-1" :: tail =>
                      parseOptions(map ++ Map('aligner1 -> true), tail)
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

/*
        val sentences = Source.fromFile(options('englishfile).asInstanceOf[String]).getLines
        val amrs = Source.fromFile(options('amrfile).asInstanceOf[String]).getLines

        for ((sentence, amrstr) <- sentences.zip(amrs)) {
            println(sentence)
            println(amrstr)
            val amr = Graph.parse(amrstr)
            if (verbosity >= 2) {
                logger(2, "AMR Graph:\n" + amr.root.prettyString(detail=2, pretty=true))
            } else {
                logger(1, "AMR Graph:\n" + amr.root.prettyString(detail=1, pretty=true))
            }
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
                logger(1, "Span "+(i+1).toString+":  "+span.words+" => "+span.amr)
                logger(3, "* "+span.format)
            }
            println(spans.map(_.format).mkString(" "))
            println()
        } */

