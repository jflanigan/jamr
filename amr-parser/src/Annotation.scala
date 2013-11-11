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
import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

case class Annotation[T](private val snt: Array[String], val tok: Array[String], val annotations: T) {
    // This class can be used for annotations on the tokens of a sentence.
    // The annotations can use a different tokenization scheme, and getSpan can be used to convert
    // a span 'tok' to a span in 'snt'.
    // annotationSpan can used to convert a span in 'snt' to a span in 'tok'.
    // Public member 'get' is the annotation.

    assert(snt.mkString == tok.mkString, "Annotation tokenization schemes do not match:\n"+snt.mkString(" ")+" != "+tok.mkString(" "))
    assert(snt.mkString.count(_ == ' ') == 0, "Spaces not allowed in tokens") // because we count spaces to find the left and right indices
    assert(tok.mkString.count(_ == ' ') == 0, "Spaces not allowed in tokens") // because we count spaces to find the left and right indices

    def makeLeftRight(tokenized: Array[String], myTokenized: Array[String]) : (Array[Int], Array[Int]) = {
        val left = myTokenized.map(x => 0)
        val right = myTokenized.map(x => 0)

        for (i <- Range(0, myTokenized.size)) {
            val regexr = myTokenized.take(i+1).mkString.split("").drop(1).mkString(" ?").r
            regexr.findPrefixOf(tokenized.mkString(" ")) match {
                case Some(prefix) => { right(i) = prefix.count(_ == ' ') + 1}
                case None => assert(false, "Error matching the prefix (this should never occur)")
            }
            if (i > 0) {
                val regexl = (myTokenized.take(i).mkString.split("").drop(1).mkString(" ?")+" ").r
                regexl.findPrefixOf(tokenized.mkString(" ")) match {
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

