package edu.cmu.lti.nlp
import scala.language.implicitConversions
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

package object amr {
/************************* Package Level Imports ********************/
    def abs = java.lang.Math.abs _
    def log = java.lang.Math.log _
    def exp = java.lang.Math.exp _
    def random = java.lang.Math.random _
    def floor = java.lang.Math.floor _
    def ceil = java.lang.Math.ceil _
    def signum = java.lang.Math.signum _
    def pow = java.lang.Math.pow _
    def min = java.lang.Math.min _
    def max = java.lang.Math.max _
    def Source = scala.io.Source
    def stdin = scala.io.Source.stdin
    //object Regex = scala.util.matching.Regex
    //type Regex = scala.util.matching.Regex
    //type Map[A,B] = scala.collection.mutable.Map
    //type Set[A] = scala.collection.mutable.Set
    //type ArrayBuffer[A] = scala.collection.mutable.ArrayBuffer[A]

/********************************************************************/
    def writeToFile(filename: String, contents: String) {       // TODO: move to a utilities file
        Files.write(Paths.get(filename), contents.getBytes(StandardCharsets.UTF_8))
    }

    implicit def AnnotationToBaseAnnotation[T](a: Annotation[T]) = a.annotations
    var verbosity = 1
    def logger(n: Int, s: Any) { if(n<=verbosity) System.err.println(s) }

    /*************** MySeq ****************/
    implicit def SeqToMySeq[T](x: Seq[T]) = new MySeq[T](x)
    class MySeq[T](private val s: Seq[T]) {
        def longestCommonPrefixLength(s2: Seq[T]) : Int = {
            // from http://stackoverflow.com/questions/8104479/how-to-find-the-longest-common-prefix-of-two-strings-in-scala
            val maxSize = min(s.size, s2.size)
            var i = 0
            while (i < maxSize && s(i) == s2(i)) {
                i += 1
            }
            return i
        }
        def getOrElse(i: Int, x: T) : T = {
            if (i < s.size) {
                s(i)
            } else {
                x
            }
        }
    }

    /************* MyArray **************/
    implicit def ArrayToMyArray[T](x: Array[T]) = new MyArray[T](x)
    class MyArray[T](private val s: Array[T]) {
        def longestCommonPrefixLength(s2: Array[T]) : Int = {
            // from http://stackoverflow.com/questions/8104479/how-to-find-the-longest-common-prefix-of-two-strings-in-scala
            val maxSize = min(s.size, s2.size)
            var i = 0
            while (i < maxSize && s(i) == s2(i)) {
                i += 1
            }
            return i
        }
        def getOrElse(i: Int, x: T) : T = {
            if (i < s.size) {
                s(i)
            } else {
                x
            }
        }
    }


    /*************** MyIterator ****************/
    /*
    implicit def IteratorToMyIterator[T](x: Iterator[T]) = new MyIterator[T](x)
    class MyIterator[T](private val it: Iterator[T]) {
        def zipAndExtend[B](it2: Iterator[B], x: T) : T = {
            if (i < array.size) {
                array(i)
            } else {
                x
            }
        }
    } */
}

