package edu.cmu.lti.nlp.amr.Generate
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

class MultiMapCount[A,B] {
    val map : Map[A,Map[B,Int]] = Map()
    def iterator : Iterator[(A,B,Int)] = {
        (for { (key, m) <- map
               (value, count) <- m } yield (key, value, count)).toIterator
    }
    def get(a: A) : Map[B,Int] = map.getOrElse(a, Map())
    def add(x: (A,B), count: Int = 1) {
        val (a, b) = x
        if (map.contains(a)) {
            val m = map(a)
            m(b) = m.getOrElse(b,0) + count
        } else {
            map(a) = Map(b -> count)
        }
    }
    def removeOne(a: A, b: B) {
        assert(map.contains(a), "Attempted to remove an element whose key is not there.")
        assert(map(a).contains(b), "Attempted to remove an element whose value is not there.")
        if (map(a)(b) <= 1) {
            map(a) -= b
        } else {
            map(a)(b) -= 1
        }
    }
    def remove(a: A, b: B) {
        assert(map.contains(a), "Attempted to remove an element whose key is not there.")
        assert(map(a).contains(b), "Attempted to remove an element whose value is not there.")
        map(a) -= b
    }
    def remove(a: A) {
        assert(map.contains(a), "Attempted to remove an element that's not there.")
        map -= a
    }
    def readFile(filename: String, aFromString: String => A, bFromString: String => B) {
        read(io.Source.fromFile(filename).getLines, aFromString, bFromString)
    }
    def read(iter: Iterator[String], aFromString: String => A, bFromString: String => B) {
        val regex = """([^\t]*)\t([^\t]*)\t([0-9]*)""".r
        map.clear()
        for (regex(f,v,c) <- iter) {
            add((aFromString(f), bFromString(v)), c.toInt)
        }
    }
    override def toString() : String = {
        val string = new StringBuilder
        for { (key, m) <- map
              (value, count) <- m } {
            string.append(key.toString + "\t" + value.toString + "\t" + count.toString + "\n")
        }
        return string.toString
    }
}

object MultiMapCount {
    def fromIterator[A,B](iter: Iterator[String], aFromString: String => A, bFromString: String => B) : MultiMapCount[A,B] = {
        val multiMap = new MultiMapCount[A,B]()
        multiMap.read(iter, aFromString, bFromString)
        return multiMap
    }
}

