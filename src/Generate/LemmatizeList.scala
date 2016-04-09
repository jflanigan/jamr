package edu.cmu.lti.nlp.amr.Generate
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.BasicFeatureVector._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}
import scala.io.Source
import java.io.BufferedWriter
import java.io.File
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import java.util.zip.GZIPOutputStream

object LemmatizeList {

    val usage = """Usage: scala -classpath . edu.cmu.lti.nlp.amr.Generate.LemmatizeList < wf_list_with_POS.txt > lemmas.txt"""
    type OptionMap = Map[Symbol, String]

    def parseOptions(map : OptionMap, list: List[String]) : OptionMap = {
        def isSwitch(s : String) = (s(0) == '-')
        list match {
            case Nil => map
            case "--help" :: tail =>                     parseOptions(map ++ Map('help -> "true"), tail)
            case "-h" :: tail =>                         parseOptions(map ++ Map('help -> "true"), tail)
            case "-v" :: value :: tail =>                parseOptions(map ++ Map('verbosity -> value), tail)

            case option :: tail => println("Error: Unknown option "+option) 
                                   sys.exit(1)
        }
    }

    def main(args: Array[String]) {
        val options = parseOptions(Map(),args.toList)
        if (options.contains('help)) { println(usage); sys.exit(1) }

        if (options.contains('verbosity)) {
            verbosity = options('verbosity).toInt
        }

        val Regex = """ *([0-9]+) ([^ ]+)_([^_]*)""".r

        val lemmaCounts : Map[String, Int] = Map()
        val wordPOSCounts : Map[(String, String), Int] = Map()
        val wordPOSLemmaCounts : Map[(String, String, String), Int] = Map()

        for (line <- Source.stdin.getLines) {
            val Regex(countStr, wordStr, pos) = line
            val count : Int = countStr.toInt
            val word = wordStr.toLowerCase
            val lemmas : List[String] = Wordnet.stemmer(word, pos).map(x => x.toLowerCase)
            for (lemma <- lemmas) {
                //System.out.println(lemma + " " + word + " " + pos + " " + count)
                lemmaCounts(lemma) = lemmaCounts.getOrElse(lemma, 0) + count
                wordPOSCounts((word, pos)) = wordPOSCounts.getOrElse((word, pos), 0) + count
                wordPOSLemmaCounts((word,pos,lemma)) = wordPOSLemmaCounts.getOrElse((word,pos,lemma), 0) + count
            }
        }

        for (((word, pos, lemma), count) <- wordPOSLemmaCounts) {
            val wordPOSGivenLemma : Double = count.toDouble / lemmaCounts(lemma).toDouble
            val lemmaGivenWordPOS : Double = count.toDouble / wordPOSCounts((word,pos)).toDouble
            System.out.println(lemma + " " + word + " " + pos + " " + count + " " + wordPOSGivenLemma + " " + lemmaGivenWordPOS)
        }
    }

}

