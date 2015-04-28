package edu.cmu.lti.nlp
import scala.language.implicitConversions
import scala.annotation.tailrec
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
        // see http://stackoverflow.com/questions/6879427/scala-write-string-to-file-in-one-statement
        Files.write(Paths.get(filename), contents.getBytes(StandardCharsets.UTF_8))
    }

    implicit def AnnotationToBaseAnnotation[T](a: Annotation[T]) = a.annotations
    var verbosity = 1
    def logger(n: Int, s: Any) { if(n<=verbosity) System.err.println(s) }


    /*********** string.splitStr ***********/
    implicit def StringToMyString(s: String) = new MyString(s)

    class MyString(private val str: String) {
        def splitStr(sep: String) : Array[String] = {
            // String.split doesn't work the way you would think.  Here is a better version.
            // See https://issues.scala-lang.org/browse/SI-5069
            splitStrToList(str, sep, List()).toArray
        }
    }

    @tailrec
    def splitStrToList(str: String, sep: String, ret : List[String]) : List[String] = {
        val i = str.indexOfSlice(sep)
        if(i == -1) {
            (str :: ret).reverse
        } else {
            val (str1, str2) = str.splitAt(i)
            val rest = str2.drop(sep.size)
            splitStrToList(rest, sep, str1 :: ret)
        }
    }


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

