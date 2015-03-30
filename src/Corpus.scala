package edu.cmu.lti.nlp.amr

import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
//import org.scalatest.Suite

object Corpus {
    def splitOnNewline(iterator: Iterator[String]) : Iterator[String] = {   // This treats more than one newline in a row as a single newline
        for {
            x <- iterator if x != ""
            p = (x :: iterator.takeWhile(_ != "").toList).mkString("\n")
        } yield p
    }

    /**
     * Takes an iterator of lines, splits on empty lines, and yields only
     * blocks of lines that contain some AMR content
     */
    def getAMRBlocks(iterator: Iterator[String]) : Iterator[String] = for (
      block <- splitOnNewline(iterator)
      if block.split("\n").exists(_.startsWith("(")) // needs to contain some AMR
    ) yield block

/*
    def getUlfString(string: String) : Map[String,String] = {
        // returns a map representation of Ulf's weird string representation
        assert(string.matches("^# ::(.|\n)*"), "This is not a valid properties string")
        val split = string.replaceAll("\n","").replaceAll("""#""","").split(" ::")
        val map = Map[String,String]()
        for (x <- split if x != "") {
            val line = x.split(" ")
            map += (("::"+line(0)) -> line.tail.mkString(" "))
        }
        return map
    }

    def toAMRTriple(input: String) : AMRTriple = {
        val lines = input.split("\n")
        val amrstr = lines.filterNot(_.matches("^#.*")).mkString(" ")
        val tokenized = lines.filter(_.matches("^# ::tok .*"))
        assert(tokenized.size == 1, "Incorrect number of tokenized ::tok ")
        val spanlines = lines.filter(_.matches("^# ::alignments .*"))
        assert(spanlines.size > 0, "Missing alignments")

        val graph = Graph.parse(amrstr)
        val sentence = getUlfString(tokenized(0))("::tok").split(" ")
        val extras = lines.filter(_.matches("^#.*")).filterNot(_.matches("^# ::alignments .*")).mkString("\n")
//        logger(2,graph.toString)
//        logger(2,sentence.toList.toString)
        var spans = ArrayBuffer[String]()
        var annotators = ArrayBuffer[String]()
        var annotation_dates = ArrayBuffer[String]()
        for (spanline <- spanlines) {
            val ulfstr : Map[String, String] = getUlfString(spanline)
//            logger(2,spanline)
            spans += ulfstr("::alignments")
            annotators += ulfstr("::annotator")
            annotation_dates += ulfstr("::date")
        }
        return AMRTriple(sentence, graph, spans, annotators, annotation_dates, lines.filterNot(_.matches("^#.*")).mkString("\n"), extras)
    } */
}

class CorpusTest /* extends Suite*/ {
    def testSplitOnNewline() {
        val split = Corpus.splitOnNewline(Iterator("a", "b", "c", "", "a", "c", "b"))
        assert(split.toList == List("a\nb\nc", "a\nc\nb"))
    }
}

