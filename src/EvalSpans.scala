package edu.cmu.lti.nlp.amr

import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator._

object EvalSpans {

    val usage = """Usage: ${JAMR_HOME}/run EvalSpans < amr_file"""
    //val usage = """Usage: scala -classpath . edu.cmu.lti.nlp.amr.EvalSpans < amr_file"""
    type OptionMap = Map[Symbol, Any]

    def parseOptions(map : OptionMap, list: List[String]) : OptionMap = {
        def isSwitch(s : String) = (s(0) == '-')
        list match {
            case Nil => map
            case "-h" :: value :: tail =>
                      parseOptions(map ++ Map('help -> value.toInt), tail)
            case "-v" :: value :: tail =>
                      parseOptions(map ++ Map('verbosity -> value.toInt), tail)
            case string :: Nil =>  println(usage); sys.exit(1)
            case option :: tail => println(usage); sys.exit(1)
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
        var n = 0

        val Alignments = """.*::alignments ([^:]*) .*""".r
        val SpanRegex = """([*]?)([0-9]+)-([0-9]+)\|(.*)""".r   // TODO: move to Span
        def normalizeSpanStr(spanstr: String) : String = {
            val SpanRegex = """([^|]*)\|(.*)""".r
            val SpanRegex(wordspan, nodestr) = spanstr
            wordspan+"|"+nodestr.split("[+]").sorted.mkString("+")
        }

        for ((block, i) <- Corpus.splitOnNewline(Source.stdin.getLines).zipWithIndex) {
            val lines = block.split("\n")
            val alignerStrs = lines.filter(x => x.matches(".*::alignments.*") && !x.matches(".*::gold.*"))
            val annotatorStrs = lines.filter(x => x.matches(".*::alignments.*") && x.matches(".*::gold.*"))

            if (alignerStrs.size != 0 && annotatorStrs.size != 0) {
                assert(lines.filter(x => x.matches("^# ::tok .*")).distinct.size == 1, "Multiple ::tok fields that are not the same, not sure which one to use.")
                val tokenized = lines.filter(x => x.matches("^# ::tok .*"))(0).drop(8).split(" ")

                logger(2,"Index: "+i.toString)
                n += 1
                val Alignments(alignerStr) = alignerStrs(alignerStrs.size-1)        // use the last alignment
                val Alignments(annotatorStr) = annotatorStrs(annotatorStrs.size-1)
                val aligner = alignerStr.split(" ").filterNot(_.matches("")).filterNot(_.matches("[*].*")).map(x => normalizeSpanStr(x)).distinct
                val annotator = annotatorStr.split(" ").filterNot(_.matches("")).filterNot(_.matches("[*].*")).map(x => normalizeSpanStr(x)).distinct
                logger(2,"aligner = "+aligner.toList.toString)
                logger(2,"annotator = "+annotator.toList.toString)
                val extra = aligner.diff(annotator)
                val missing = annotator.diff(aligner)
                for (i <- missing) {
                    val SpanRegex(corefStr, start, end, nodeStr) = i
                    val blah : String = tokenized.slice(start.toInt, end.toInt).mkString(" ")
                    //logger(1,"MISSINGt: "+tokenized.slice(start.toInt, end.toInt).toList.map(x => {val y: String = x; y }).toString)
                    logger(1,"MISSING: "+tokenized.slice(start.toInt, end.toInt).map(_.toString).mkString(" ").toString+" = "+i)
                }
                for (i <- extra) {
                    val SpanRegex(corefStr, start, end, nodeStr) = i
                    //logger(1,"EXTRAt: "+tokenized.slice(start.toInt, end.toInt).toList.map(x => {val y: String = x; y }).toString)
                    //logger(1,"EXTRAt: "+tokenized.slice(start.toInt, end.toInt).toList.toString)
                    logger(1,"EXTRA:   "+tokenized.slice(start.toInt, end.toInt).map(_.toString).mkString(" ").toString+" = "+i)
                }
                logger(2,"correct = "+annotator.diff(annotator.diff(aligner)).toList.toString)
                correct += annotator.diff(annotator.diff(aligner)).size
                aligner_total += aligner.size
                gold_total += annotator.size
            }
        }

        logger(2,"correct = "+correct.toString)
        logger(2,"aligner_total = "+aligner_total.toString)
        logger(2,"gold_total = "+gold_total.toString)

        val p = correct/aligner_total
        val r = correct/gold_total
        val f1 = 2.0*p*r/(p+r)

        println("Number of AMR: "+n.toString)
        println("Precision = "+p.toString)
        println("Recall = "+r.toString)
        println("F1 = "+f1.toString)
    }
}


