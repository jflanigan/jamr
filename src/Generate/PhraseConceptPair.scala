package edu.cmu.lti.nlp.amr.Generate
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

// Example: Phrase(List(("#", "(X thing)"), ("ARG1OF", "(ARG1OF offer-01)")), "offer", "NN", "NN")

case class PhraseConceptPair(words: String, graphFrag: String, pos: String, headPos: String) {
    def mkRule : String = {
       return Rule.mkLhs(Graph.parse(graphFrag), sameSpan=false)+") ||| "+words
    }
    def lhs : List[(String, String)] = {
        return Rule.mkLhsList(Graph.parse(graphFrag), sameSpan=false)
    }
}

object PhraseConceptPair {
    def PhraseConceptPair(span: Span, pos: Array[String]) : PhraseConceptPair = {
        PhraseConceptPair(span.words, span.amr.toString, pos.slice(span.start, span.end).mkString(" "), pos.slice(span.end-1, span.end))
    }
}

