package edu.cmu.lti.nlp.amr

import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
//import org.scalatest.Suite

case class AMRTriple(sentence: Array[String], graph: Graph, spans: Array[ArrayBuffer[Span]], annotators: Array[String])

object Corpus {
    def splitOnNewline(iterator: Iterator[String]) : Iterator[String] = {   // This treats more than one newline in a row as a single newline
        for {
            x <- iterator if x != ""
            p = (x :: iterator.takeWhile(_ != "").toList).mkString
        } yield p
    }

    def getUlfString(string: String) : Map[String,String] = {
        // returns a map representation of Ulf's weird string representation
        assert(string.matches("^# ::.*"), "This is not a valid properties string")
        val split = string.replaceAll("""#""","").split(" ::")
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
        val spanlines = lines.filter(_.matches("^# ::alignment .*"))
        assert(spanlines.size > 0, "Missing alignments")
        
        val graph = Graph.parse(amrstr)
        //val TokExtractor = "^# ::tok (.*)".r
        //val ("^# ::tok (.*)".r)(sentence) = tokenized(0)
        //val SpanExtractor = "^ ::alignment ([^:])+ ::annotator ([^:])".r

        val sentence = getUlfString(tokenized(0))("::tok").split(" ")
        var spans = List[ArrayBuffer[Span]]()
        var annotators = List[String]()
        for (spanline <- spanlines) {
            val ulfstr : Map[String, String] = getUlfString(spanline)
            val newspan : ArrayBuffer[Span] = Span.readSpans(ulfstr("::alignment"), graph, sentence)
            spans = newspan :: spans
            annotators = (ulfstr("::annotator") + " " + ulfstr("::date")) :: annotators
        }
        return AMRTriple(sentence, graph, spans.reverse.toArray, annotators.reverse.toArray)
    }
}

class CorpusTest /* extends Suite*/ {
    def testSplitOnNewline() {
        val split = Corpus.splitOnNewline(Iterator("a", "b", "c", "", "a", "c", "b"))
        assert(split.toList == List("abc", "acb"))
    }
    def testGetUlfString() {
        val map1 = Corpus.getUlfString("# ::snt testing 1 2 3")
        val map2 = Corpus.getUlfString("# ::date 12 15 2005 ::annotator Jeff ::preferred")
        assert(map1 == Map("::snt" -> "testing 1 2 3"))
        assert(map2 == Map("::date" -> "12 15 2005", "::annotator" -> "Jeff", "::preferred" -> ""))
    }
}

