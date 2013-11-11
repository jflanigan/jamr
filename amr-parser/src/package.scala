package edu.cmu.lti.nlp
import scala.language.implicitConversions
import java.lang.Math.min

package object amr {
    implicit def doubleToMulAssoc(x: Double) = new MulAssoc(x)
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

