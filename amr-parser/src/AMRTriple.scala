package edu.cmu.lti.nlp.amr

import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
//import org.scalatest.Suite

import edu.cmu.lti.nlp.amr.GraphDecoder.Input

// TODO: A better name for this would be AMRData, or SpanData

// AMRTriple holds the (possibly multiple) span annotations for a sentence and graph pair
// An element in spans is a span string (i.e. "1-2|0 0-1|0.0 2-3|0.1 4-5|0.2")
case class AMRTriple(sentence: Array[String], graph: Graph, spans: ArrayBuffer[String], annotators: ArrayBuffer[String], annotation_dates: ArrayBuffer[String], amrStr: String, extras: String) {
    def toInput(): Input = {
        // WARNING: this function modifies the graph
        toOracle
        graph.clearEdges
        return Input(graph, sentence, Array(), Array())
    }
    def toOracle(): Input = {
        // WARNING: this function modifies the graph
        val annotationIndex = annotators.size - 1
        graph.loadSpans(spans(annotationIndex), sentence)
        return Input(graph, sentence, Array(), Array())
    }
}

