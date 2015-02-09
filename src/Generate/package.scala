package edu.cmu.lti.nlp.amr
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

package object Generate {
    def Label(label: String) : String = {
        return label.drop(1).toUpperCase.replaceAll("-","_")
    }
    //def labelStr(label: String) = Label(label)
    def escape(str: String, esc: Char = '\\') : String = {
        return str.replaceAllLiterally(esc.toString, esc.toString + esc.toString)
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
        return splitStr(unEscape(str, esc), "\t")
    }
    def projectPos(posAnno: Annotation[Array[String]]) : Array[String] = {
        val sentence : Array[String] = posAnno.snt // tokenized sentence
        val pos = (0 until sentence.size).map(i => {
            val span = posAnno.annotationSpan(i,i+1)
            val posList = posAnno.annotation.slice(span._1, span._2)    // should always return a non-empty array
            posList.last    // take the last one (works well in English)
        }).toArray
        return pos
    }
    def splitStr(str: String, sep: String) : Array[String] = {
        // String.split doesn't work the way you would think.  Here is a better version.
        // See https://issues.scala-lang.org/browse/SI-5069
        splitStrToList(str, sep).toArray
    }
    def splitStrToList(str: String, sep: String) : List[String] = {
        val i = str.indexOfSlice(sep)
        if(i == -1) {
            List(str)
        } else {
            val (str1, str2) = str.splitAt(i)
            val rest = str2.drop(sep.size)
            str1 :: splitStrToList(rest, sep)
        }
    }
}

