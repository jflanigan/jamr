package edu.cmu.lti.nlp.amr.Generate
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

// Example: Phrase(List(("#", "(X thing)"), ("ARG1OF", "(ARG1OF offer-01)")), "offer", "NN", "NN")

case class Phrase(lhs: List[(String, String)], words: String, pos: String, headPos: String) {
    def mkRule : String = {
        "(X "+lhs.sortBy(_._1).map(_._2).mkString(" ")+") ||| "+words
    }
}

object Phrase {
    def Phrase(span: Span, pos: Array[String]) : Phrase = { 
        Phrase(Rule.mkLhsList(span.node), span.words, pos.slice(span.start, span.end).mkString(" "))
    }
}

