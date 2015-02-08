package edu.cmu.lti.nlp.amr.Generate
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

case class Arg(left: String, label: String, right: String) {
    val tag : String = left.replaceAllLiterally(" ","_")+"|"+label+"|"+right.replaceAllLiterally(" ","_")
    def ruleStr(index: Int) : String = { left + " [" + index.toString +"] " + right }
    override def toString : String = { left + " [" + label +"] " + right }
    def serialize : String = { escape(left, '|') + "|" + escape(label, '|') + "|" + escape(right, '|') }
}

object Arg {
    def apply(string: String) : Arg = { logger(0,"ARG = "+string); val f = unEscapeArray(string,'|'); logger(0,"unEscaped = "+f.toList); Arg(f(0), f(1), f(2)) }    // deserialize
    val START : Arg = Arg("", "<START>", "")
    val STOP : Arg = Arg("", "<STOP>", "")
    val CONCEPT : Arg = Arg("", "<CONCEPT>", "")
    def Default(label: String) : Arg = Arg("", label, "")   // default pass through no words left or right
}

