package edu.cmu.lti.nlp.amr

import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator._

case class Span(var start: Int, var end: Int, var nodeIds: List[String], var words: String, var amr: Node, var coRef: Boolean) {
    def format() : String = {
        if (start < end) {
            coRef match {
                case false => start.toString+"-"+end.toString+"|"+nodeIds.mkString("+")
                case true => "*"+start.toString+"-"+end.toString+"|"+nodeIds.mkString("+")
            }
        } else {
            ""
        }
    }
}

