package edu.cmu.lti.nlp.amr
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

package object Generate {
    def Label(label: String) : String = {
        return label.drop(1).toUpperCase.replaceAll("-","_")
    }
    def labelStr(label: String) = Label(label)
    def escape(str: String, esc: Char = '\\') : String = {
        return str.replaceAllLiterally(esc.toString, esc.toString + sec.toString)
    }
    def escape(str: String, esc: String) : String = {
        var s = str
        for (c <- esc) {
            s = s.replaceAllLiterally(esc.toString, esc.toString + esc.toString)
        }
        return s
    }
    def unEscape(str: String, esc: Char = '\\') : String = {    // unescapes to a tab
        return str.replaceAllLiterally(esc.toString, "\t").replaceAllLiterally("\t\t", esc.toString)
    }
    def unEscape(str: String, esc: String) : String = {         // unescapes to a tab
        var s = str
        for (c <- esc) {
            s = s.replaceAllLiterally(esc.toString, "\t")
            s = s.replaceAllLiterally("\t\t", esc.toString)
        }
        return s
    }
    def unEscapeArray(str: String, esc: Char) : Array[String] = {
        return unEscape(str, esc).split("\t")
    }
}

