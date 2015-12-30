package edu.cmu.lti.nlp.amr

import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable => m, immutable => i}
import scala.collection.mutable.PriorityQueue

abstract class SemiRing[T] {
    def times(a: T) : T   // Tropical semiring plus
    def plus(a: T) : T    // Tropical semiring max
}

case class Tropical[S](val path: List[S], val score: Double) extends SemiRing[Tropical[S]] {
    def times(a: Tropical[S]) : Tropical[S] = {
        return new Tropical(path ::: a.path, score + a.score)
    }
    def plus(a: Tropical[S]) : Tropical[S] = {
        return List(this,a).maxBy(_.score)
    }
}

object Tropical {
    def Zero[S] : Tropical[S] = {   // additive identity
        new Tropical[S](List(), Double.NegativeInfinity)
    }
    def One[S] : Tropical[S] = {    // multiplicative identity
        new Tropical[S](List(), 0.0)
    }
    def apply[S](s: S, score: Double) : Tropical[S] = {
        new Tropical[S](List(s), score)
    }
    def make[S](s: S, score: Double) : Tropical[S] = apply[S](s, score)
}

case class KBest[S](val kbest: List[Tropical[S]], // assumes kbest is always sorted
               val k: Int) extends SemiRing[KBest[S]] {

    def times(a: KBest[S]) : KBest[S] = {
        assert(k == a.k, "Warning: kbest sizes differ.")
        //logger(0, "KBest.times")
        //logger(0, "this = "+this)
        //logger(0, "a = "+a)
        val queue = new PriorityQueue[Tropical[S]]()(Ordering.by(x => x.score))
        for { x <- kbest
              y <- a.kbest
            } {
                queue.enqueue(x.times(y))
        }
        return new KBest(queue.take(k).toList, k)
    }

    def plus(a: KBest[S]) : KBest[S] = {
        assert(k == a.k, "Warning: kbest sizes differ.")
        return new KBest((kbest ::: a.kbest).sortBy(x => -x.score).take(k), k)  // could use min(k, a.k) and not throw error
    }
}

object KBest {
    def Zero[S](k: Int) : KBest[S] = {  // additive identity
        new KBest[S](List(), k)
    }
    def One[S](k: Int) : KBest[S] = {   // multiplicative identity
        new KBest[S](List(Tropical.One[S]), k)
    }
    def apply[S](k: Int)(s: S, score: Double) : KBest[S] = {
        new KBest[S](List(new Tropical(List(s), score)), k)
    }
}

