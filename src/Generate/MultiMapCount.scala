package edu.cmu.lti.nlp.amr.Generate
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

class MultiMapCount[A,B] {
    val map : Map[A,Map[B,Int]] = Map()
    def add(x: (A,B), count: Int) {
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
}


