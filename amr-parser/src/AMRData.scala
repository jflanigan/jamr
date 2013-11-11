package edu.cmu.lti.nlp.amr

import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
//import org.scalatest.Suite

import edu.cmu.lti.nlp.amr.GraphDecoder.Input

// TODO: A better name for this would be AMRData, or SpanData

// AMRData holds the (possibly multiple) span annotations for a sentence and graph pair
// An element in spans is a span string (i.e. "1-2|0 0-1|0.0 2-3|0.1 4-5|0.2")
case class AMRData(sentence: Array[String], graph: Graph, spans: ArrayBuffer[String], annotators: ArrayBuffer[String], annotation_dates: ArrayBuffer[String], amrStr: String, extras: String) {
    def toInputGraph(): Graph = {
        // WARNING: this function modifies the graph
        val annotationIndex = annotators.size - 1
        logger(1,"Sentence = " + sentence.toList)
        logger(1,"span = " + spans(annotationIndex))
        graph.loadSpans(spans(annotationIndex), sentence)
        graph.clearEdges
        graph.normalizeInverseRelations
        graph.addVariableToSpans
        return graph
    }
    def toOracleGraph(clearUnalignedNodes : Boolean): Graph = {
        // WARNING: this function modifies the graph
        val annotationIndex = annotators.size - 1
        graph.loadSpans(spans(annotationIndex), sentence)
        if (clearUnalignedNodes) {
            graph.clearUnalignedNodes
        }
        graph.normalizeInverseRelations
        graph.addVariableToSpans
        return graph
    }
}

object AMRData {
    private def getUlfString(string: String) : Map[String,String] = {
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

    def apply(input: String) : AMRData = {
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
        return AMRData(sentence, graph, spans, annotators, annotation_dates, lines.filterNot(_.matches("^#.*")).mkString("\n"), extras)
    }

    class AMRDataTest /* extends Suite*/ {
        def testGetUlfString() {
            val map1 = AMRData.getUlfString("# ::snt testing 1 2 3")
            val map2 = AMRData.getUlfString("# ::date 12 15 2005 ::annotator Jeff ::preferred")
            assert(map1 == Map("::snt" -> "testing 1 2 3"))
            assert(map2 == Map("::date" -> "12 15 2005", "::annotator" -> "Jeff", "::preferred" -> ""))
        }
    }
}


