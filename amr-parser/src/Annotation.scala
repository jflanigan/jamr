package edu.cmu.lti.nlp.amr

import java.io.File
import java.io.FileOutputStream
import java.io.PrintStream
import java.io.BufferedOutputStream
import java.io.OutputStreamWriter
import java.lang.Math.abs
import java.lang.Math.log
import java.lang.Math.exp
import java.lang.Math.random
import java.lang.Math.floor
import java.lang.Math.min
import java.lang.Math.max
import java.util.regex.Pattern
import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

case class Annotation[T](private val snt: Array[String], val tok: Array[String], var annotation: T) {
    // This class can be used for annotations on the tokens of a sentence.
    // The annotations can use a different tokenization scheme, and getSpan can be used to convert
    // a span 'tok' to a span in 'snt'.
    // annotationSpan can used to convert a span in 'snt' to a span in 'tok'.
    // Public member 'annotation' is the annotation.

    assert(normalizedStr(snt,"") == normalizedStr(tok,""), "Tokenization schemes do not match. This may be an error with the parser, the input to the parser, or the POS tagger, or incorrect handling of Unicode characters by either. The offending line is:\n\n"+snt.mkString(" ")+" != "+tok.mkString(" ")+"\n"/*+"\nwhich was normalized to:\n"+normalizedStr(snt,"")+" != "+normalizedStr(tok,"")*/)
    assert(snt.mkString.count(_ == ' ') == 0, "Spaces not allowed in tokens") // because we count spaces to find the left and right indices
    assert(tok.mkString.count(_ == ' ') == 0, "Spaces not allowed in tokens") // because we count spaces to find the left and right indices

    def annotations: T = annotation   // alias for annotation
    def annotations_= (a: T) { annotation = a }

    def normalizedRegex(tokens: Array[String]) : String = {
        // For some reason, Pattern.quote doesn't seem to work when we have "(" or ")" in our input, so we change them to "-LRB-" and "-RRB-" (and do the same in normalizedStr)
        // The Stanford parser replaces unicode with ??? so, we delete any ?
        // We also remove any unicode
        normalizedStr(tokens, "").split("").map(x => Pattern.quote(x)).drop(1).mkString(" ?")
        //normalizedStr(tokens, "").split("").map(x => Pattern.quote(x)).drop(1).mkString(" *")   // * because we may have removed a token completely using the replacement rules, so we must match "  "
    }
    def normalizedStr(tokens: Array[String], spacer: String = " ") : String = {
        tokens.mkString(spacer).replaceAllLiterally("(","-LRB-").replaceAllLiterally(")","-RRB-")
        //tokens.mkString(spacer).replaceAllLiterally("(","-LRB-").replaceAllLiterally(")","-RRB-").replaceAllLiterally("?", "").replaceAll("\\P{Print}", "")
    }

    def makeLeftRight(tokenized: Array[String], myTokenized: Array[String]) : (Array[Int], Array[Int]) = {
        val left = myTokenized.map(x => 0)
        val right = myTokenized.map(x => 0)

        for (i <- Range(0, myTokenized.size)) {
            val regexr = normalizedRegex(myTokenized.take(i+1)).r
            logger(2, "regexr = "+regexr)
            logger(2, "tokenized = "+normalizedStr(tokenized))
            regexr.findPrefixOf(normalizedStr(tokenized)) match {
                case Some(prefix) => { right(i) = prefix.count(_ == ' ') + 1}
                case None => assert(false, "Error matching the prefix (this should never occur)")
            }
            if (i > 0) {
                val regexl = (normalizedRegex(myTokenized.take(i))+" ").r
                regexl.findPrefixOf(normalizedStr(tokenized)) match {
                    case Some(prefix) => { left(i) = right(i-1) }
                    case None => { left(i) = right(i-1) - 1 }
                }
            }
        }
        //println(right.toList)
        //println(left.toList)
        return (left, right)
    }

    val (left, right) = makeLeftRight(snt, tok)
    val (leftinv, rightinv) = makeLeftRight(tok, snt)

    def getSpan(span: (Int, Int)) : (Int, Int) = {
        // Input: Span in tok (indices start from 0, and span goes from span._1 to span._2-1 inclusive ie same as slice(Int,Int))
        // Output: Span in snt (indices start from 0, and span goes from ret._1 to ret._2-1 inclusive ie same as slice(Int,Int))
        return (left(span._1), right(span._2-1))
    }

    def annotationSpan(span: (Int, Int)) : (Int, Int) = {
        // Input: Span in snt
        // Output: Span in tok
        return (leftinv(span._1), rightinv(span._2-1))
    }
}

object Annotation {
class AnnotationTest /*extends Suite*/ {
    def testGetSpan() {
        val test1 = Annotation[Array[String]](Array("The", "Riyadh", "-", "based", "Naif", "Arab", "Academy"),
                                              Array("The", "Riyadh-based", "Naif", "Arab", "Academy"),
                                              Array())
        println("test1")
        println("tokenized = "+test1.snt.toList)
        println("myTokenized = "+test1.tok.toList)
        for (i <- Range(0, test1.tok.size)) {
            println("i=" + i + " l=" + test1.getSpan(i,i+1)._1 + " r=" + test1.getSpan(i,i+1)._2)
        }
        val test2 = Annotation[Array[String]](Array("The", "Riyadh-based", "NaifArab", "Academy"),
                                              Array("The", "Riyadh", "-", "based", "Naif", "ArabAcademy"),
                                              Array())
        println("test2")
        println("tokenized = "+test2.snt.toList)
        println("myTokenized = "+test2.tok.toList)
        for (i <- Range(0, test2.tok.size)) {
            println("i=" + i + " l=" + test2.getSpan(i,i+1)._1 + " r=" + test2.getSpan(i,i+1)._2)
        }
        println("getSpan(1,4)="+test2.getSpan(1,4))
        println("getSpan(1,5)="+test2.getSpan(1,5))
        val test3 = Annotation[Array[String]](Array("The", "Riyadh-", "based", "NaifArab", "Academy"),
                                              Array("The", "Riy", "adh", "-", "based", "Naif", "ArabAcademy"),
                                              Array())
        println("test3")
        println("tokenized = "+test3.snt.toList)
        println("myTokenized = "+test3.tok.toList)
        for (i <- Range(0, test3.tok.size)) {
            println("i=" + i + " l=" + test3.getSpan(i,i+1)._1 + " r=" + test3.getSpan(i,i+1)._2)
        }

    }
}
}

