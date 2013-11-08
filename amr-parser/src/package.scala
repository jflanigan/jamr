package edu.cmu.lti.nlp
import scala.language.implicitConversions

package object amr {
    implicit def doubleToMulAssoc(x: Double) = new MulAssoc(x)
    var verbosity = 1
    def logger(n: Int, s: Any) { if(n<=verbosity) System.err.println(s) }


    /*************** MyArray ****************/
    implicit def ArrayToMyArray[T](x: Array[T]) = new MyArray[T](x)
    class MyArray[T](private val array: Array[T]) {
        def getOrElse(i: Int, x: T) : T = {
            if (i < array.size) {
                array(i)
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

