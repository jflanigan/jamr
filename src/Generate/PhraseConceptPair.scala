package edu.cmu.lti.nlp.amr.Generate
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

// Example: Phrase("offer", "offer-01", "NN", "NN")
// Example: offer ||| offer-01 ||| NN ||| NN

// I know this class is duplicated in ConceptInvoke.PhraseConceptPair.
// There are some slight differences between the two because the application is different.

case class PhraseConceptPair(words: String, graphFrag: String, fullPos: String, headPos: String) {
    // graphFrag shouldn't contain variable names.  Example:
    //   (date-entity :day 5 :month 1 :year 2002)
    // This is necessary so that phraseConcept pairs are identified as the same regardless
    // of ambiguities in variable names.  Remember, the graphFrag is a tree so re-entrancies
    // are not necessary.
    def mkRule : String = {
       return Rule.mkLhs(Graph.parse(graphFrag), sameSpan=false)+") ||| "+words
    }
    def lhs : List[(String, String)] = {
        return Rule.mkLhsList(Graph.parse(graphFrag), sameSpan=false)
    }
    def realization : String = words
    override def toString : String = {
        return words + " ||| " + graphFrag + " ||| " + fullPos + " ||| " + headPos
    }
}

object PhraseConceptPair {
    def fromSpan(span: Span, pos: Array[String]) : PhraseConceptPair = {
        // Assumes head final for calculating headPos (ok heuristic for English)
        return PhraseConceptPair(span.words,
                                 span.amr.prettyString(0, false, Set.empty[String]) /*no variable names*/
                                 pos.slice(span.start, span.end).mkString(" "),
                                 pos.slice(span.end-1, span.end))
    }

    def apply(string: String) : Rule = {    // TODO: could also do unapply, so you can do val Rule(...) = string
        val regex = """([^|]*) \|\|\| (.*) \|\|\| ([^\|]*) \|\|\| ([^|]*)""".r
        val regex(words, graphFrag, fullPos, headPos) = string
        return PhraseConceptPair(words, graphFrag, fullPos, headPos)
    }
}

