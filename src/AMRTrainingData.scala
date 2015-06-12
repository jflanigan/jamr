package edu.cmu.lti.nlp.amr

import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
//import org.scalatest.Suite

// AMRTrainingData holds the (possibly multiple) span annotations for a sentence and 
// AMR graph pair
// An element in spans is a span string (i.e. "1-2|0 0-1|0.0 2-3|0.1 4-5|0.2")
case class AMRTrainingData(sentence: Array[String], graph: Graph, spans: ArrayBuffer[String], annotators: ArrayBuffer[String], annotation_dates: ArrayBuffer[String], amrStr: String, extras: String) {
    def toInputGraph(): Graph = { // Input to stage 2
        // WARNING: this function modifies the graph
        val annotationIndex = annotators.size - 1   // use the last alignments listed (if there are multiple listed)
        logger(1,"Sentence = " + sentence.toList)
        logger(1,"span = " + spans(annotationIndex))
        graph.loadSpans(spans(annotationIndex), sentence)
        graph.clearEdges
        graph.normalizeInverseRelations
        graph.addVariableToSpans
        logger(1, "InputGraph nodes = "+graph.nodes.map(x => x.concept).toList)
        logger(1, "InputGraph triples: "+graph.printTriples(detail = 1))
        return graph
    }
    def toOracleGraph(clearUnalignedNodes : Boolean): Graph = { // Stage2 oracle
        // WARNING: this function modifies the graph
        val annotationIndex = annotators.size - 1   // use the last alignments listed (if there are multiple listed)
        graph.loadSpans(spans(annotationIndex), sentence)
        if (clearUnalignedNodes) {
            graph.clearUnalignedNodes
        }
        graph.normalizeInverseRelations
        graph.addVariableToSpans
        logger(2, "OracleGraph nodes = "+graph.nodes.map(x => x.concept).toList)
        logger(2, "OracleGraph triples: "+graph.printTriples(detail = 1))
        return graph
    }
    def loadSpans() {
        val annotationIndex = annotators.size - 1
        graph.loadSpans(spans(annotationIndex), sentence)
    }
}

object AMRTrainingData {
    def getUlfString(string: String) : Map[String,String] = {
        // returns a map representation of Ulf's string representation
        assert(string.startsWith("# ::"), "This is not a valid properties string")
        val split = string.replaceAll("\n","").replaceAll("""#""","").split(" ::")
        val map = Map[String,String]()
        for (x <- split if x != "") {
            val line = x.split(" ")
            map += (("::"+line(0)) -> line.tail.mkString(" "))
        }
        return map
    }

    def apply(input: String) : AMRTrainingData = {
        val lines = input.split("\n")
        val amrstr = lines.filterNot(_.matches("^#.*")).mkString(" ")
        val tokenized = lines.filter(_.matches("^# ::tok .*"))
        assert(tokenized.size == 1, "Incorrect number of tokenized ::tok.\nInput:\n"+input)
        val spanlines = lines.filter(_.matches("^# ::alignments .*"))
        assert(spanlines.size > 0, "Missing alignments\nInput:\n"+input)

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
            annotators += ulfstr.getOrElse("::annotator", "")
            annotation_dates += ulfstr.getOrElse("::date", "")
        }
        return AMRTrainingData(sentence, graph, spans, annotators, annotation_dates, lines.filterNot(_.matches("^#.*")).mkString("\n"), extras)
    }

    class AMRTrainingDataTest /* extends Suite*/ {
        def testGetUlfString() {
            val map1 = AMRTrainingData.getUlfString("# ::snt testing 1 2 3")
            val map2 = AMRTrainingData.getUlfString("# ::date 12 15 2005 ::annotator Jeff ::preferred")
            assert(map1 == Map("::snt" -> "testing 1 2 3"))
            assert(map2 == Map("::date" -> "12 15 2005", "::annotator" -> "Jeff", "::preferred" -> ""))
        }
    }
}


