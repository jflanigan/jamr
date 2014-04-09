package edu.cmu.lti.nlp.amr

import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator._

/****************************** Lazy Array *****************************/

class LazyArray[T](iterator : Iterator[T]) {
    private var buffer = ArrayBuffer[T]()
    def apply(index: Int) : T = {
        while (index >= buffer.size) {
            buffer.+=(iterator.next)
        }
        buffer.apply(index)
    }
    def map[B](f: T => B) : LazyArray[B] = {
        buffer ++= iterator
        val newBuffer = buffer.map(f)
        val newArray = LazyArray(iterator.map(f))     // I don't know how to concatenate two iterators
                                                      // Otherwise I would convert the buffer to an iterator, map it and then concatenate the iterators
        newArray.buffer = newBuffer
        return newArray
    }
    def foreach(f: T => Unit) : Unit = {
        buffer.foreach(f)
        while (iterator.hasNext) {
            val x = iterator.next
            buffer += x
            f(x)
        }
    }
    def loadEverything() {
        while(iterator.hasNext) {
            buffer.+=(iterator.next)
        }
    }
}

object LazyArray {
    def apply[T](iterator : Iterator[T]) : LazyArray[T] = {
        return new LazyArray[T](iterator)
    }
}
