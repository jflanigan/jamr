package edu.cmu.lti.nlp.amr.Generate
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

case class ConceptInfo(realization: PhraseConceptPair, position: Int) {
    override def toString : String = {
        realization.toString + " ||| " + position.toString
    }
}

object ConceptInfo {
    def apply(string: String) : ConceptInfo = {
        val splitted = string.splitStr(" ||| ")
        return ConceptInfo(PhraseConceptPair(splitted.init.mkString(" ||| ")), splitted.last.toInt)
    }
}

