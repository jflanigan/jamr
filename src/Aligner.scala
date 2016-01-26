package edu.cmu.lti.nlp.amr

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}
import java.util.Date
import java.text.SimpleDateFormat

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
            case "--print-nodes-and-edges" :: tail =>
                      parseOptions(map ++ Map('printNodesAndEdges -> true), tail)
            case "--log-unaligned" :: tail =>
                      parseOptions(map ++ Map('logUnalignedConcepts -> true), tail)
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

        var aligner3 = true
        if (options.contains('aligner1)) {
            aligner3 = false
        }

        val sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS")
        for (block <- Corpus.splitOnNewline(Source.stdin.getLines)) {
            if (block.split("\n").exists(_.startsWith("("))) {  // Does it contain some AMR?
                logger(2,"**** Processsing Block *****")
                logger(2,block)
                logger(2,"****************************")
                val extrastr : String = block.split("\n").filter(_.matches("^# ::.*")).mkString("\n")
                val amrstr : String = block.split("\n").filterNot(_.matches("^#.*")).mkString("\n")
                println(extrastr)
                val amr = Graph.parse(amrstr)
                val extras = AMRTrainingData.getUlfString(extrastr)
                val tokenized = extras("::tok").split(" ")
                val wordAlignments = AlignWords.alignWords(tokenized, amr)
                val spanAlignments = if (aligner3) {
                        AlignSpans3.align(tokenized, amr)
                    } else {
                        AlignSpans.alignSpans(tokenized, amr, wordAlignments)
                    }
                val spans = amr.spans
                for ((span, i) <- spans.zipWithIndex) {
                    logger(1, "Span "+(i+1).toString+":  "+span.words+" => "+span.amr)
                    logger(3, "* "+span.format)
                }
                if (aligner3) {
                    println("# ::alignments "+spans.map(_.format).mkString(" ")+" ::annotator Aligner v.03 ::date "+sdf.format(new Date))
                } else {
                    println("# ::alignments "+spans.map(_.format).mkString(" ")+" ::annotator Aligner v.01 ::date "+sdf.format(new Date))
                }
                if (options.contains('logUnalignedConcepts)) {
                    amr.logUnalignedNodes()
                }
                if (options.contains('printNodesAndEdges)) {
                    println(amr.printNodes.map(x => "# ::node\t" + x).mkString("\n"))
                    println(amr.printRoot)
                    if (amr.root.relations.size > 0) {
                        println(amr.printEdges.map(x => "# ::edge\t" + x).mkString("\n"))
                    }
                }
                println(amrstr+"\n")
            } else {
                println(block+"\n")
            }
        }
    }

}

