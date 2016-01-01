package edu.cmu.lti.nlp.amr.Generate.SyntheticRules
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

import scala.reflect.ClassTag

/******************************** Viterbi Decoder *******************************/

object SemiRingViterbi {
    private case class State(prev: Int, cur: Int, i: Int)

    def decode_1st_order[T:ClassTag](tags: Array[Array[T]],
                           localScore: (T,T,Int) => Double) : (List[T], Double) = {
        // for why ClassTag is needed, see http://stackoverflow.com/questions/16921168/scala-generic-method-no-classtag-available-for-t
        def score(state: State) : Double = {
            val i = state.i
            localScore(tags(i-1)(state.prev), tags(i)(state.cur), i)
        }
        val result : Tropical[Int] = decode_1st_order[Tropical[Int]](tags.size, score _, (i: Int) => tags(i).size, Tropical.make[Int] _)
        val resultTags : List[T] = result.path.zipWithIndex.map(x => tags(x._2)(x._1))
        return (resultTags, result.score)
    }

    def kbest_1st_order[T:ClassTag](tags: Array[Array[T]],
                          localScore: (T,T,Int) => Double,
                          k: Int) : List[(List[T], Double)] = {
        // for why ClassTag is needed, see http://stackoverflow.com/questions/16921168/scala-generic-method-no-classtag-available-for-t
        def score(state: State) : Double = {
            val i = state.i
            localScore(tags(i-1)(state.prev), tags(i)(state.cur), i)
        }
        val kBest : List[(List[T], Double)] =
            for { result <- decode_1st_order[KBest[Int]](tags.size, score _, (i: Int) => tags(i).size, KBest[Int](k) _).kbest
                } yield {
                    //logger(0, "tags.size = " + tags.size.toString)
                    //logger(0, "tag sizes = " + tags.map(x => x.size).toList)
                    //logger(0, "result.path = " + result.path)
                    val resultTags : List[T] = result.path.zipWithIndex.map(x => tags(x._2)(x._1))
                    //logger(0, "resultTags.size = " + resultTags.size.toString)
                    //logger(0, "resultTags = " + resultTags.toList)
                    (resultTags, result.score)
            }
        return kBest
    }

    private def decode_1st_order[SemiRingElem <: SemiRing[SemiRingElem] : ClassTag](length: Int,
                                     localScore: State => Double,
                                     tags: Int => Int,
                                     SemiRingElem: (Int, Double) => SemiRingElem) : SemiRingElem = {
        // Viterbi algorithm modified from Fig 5.17 in Speech & Language Processing (p. 147)
        //   length: the length of the input (INCLUDING start and stop padding)
        //   localScore: State(prevState, curState, position) => transition weight (log prob)
        //   tags: position => number of tags
        //   SemiRingElem: constructs a semiring element from a tag (Int) and a score (Double)
        // returns the Viterbi semiring element
        //logger(0, "length = " + length.toString)
        assert(length > 2, "Length must be greater than 2")
        assert(tags(0) == 1, "There must be a single start tag")
        assert(tags(length-1) == 1, "There must be a single stop tag")

        val T = length
        val viterbi = new Array[Array[SemiRingElem]](T)

        def max_prev(t: Int, sCur: Int) : SemiRingElem = {
            // Find the most likely previous state (highest model score)
            val scores : List[SemiRingElem] = Range(0,tags(t-1)).map(sPrev => viterbi(t-1)(sPrev).times(SemiRingElem(sCur, localScore(State(sPrev,sCur,t))))).toList
            return (scores.tail :\ scores.head)((a: SemiRingElem, b: SemiRingElem) => a.plus(b))
        }

        // Initialize (t = 0)
        viterbi(0) = new Array(1)
        viterbi(0)(0) = SemiRingElem(0, 0.0)    // this is not the multiplicative identity (SemiRingOne)
        //logger(0, "viterbi(0) = "+viterbi(0).toList)
        //viterbi(0) = Array(SemiRingElem(0, 0.0))

        // Recursive
        for (t <- Range(1, T-1)) {
            viterbi(t) = new Array[SemiRingElem](tags(t))
            for (s <- Range(0, tags(t))) {
                viterbi(t)(s) = max_prev(t, s)
            }
            //logger(0, "viterbi("+t+") = "+viterbi(t).toList)
        }
        // Termination
        //logger(0, "viterbi("+(T-1).toString+") = "+max_prev(T-1, 0))

        return max_prev(T-1, 0)
    }

    def kbest_0th_order[T:ClassTag](tags: Array[Array[T]],
                          localScore: (T,T,Int) => Double,
                          k: Int) : List[(List[T], Double)] = {
        // for why ClassTag is needed, see http://stackoverflow.com/questions/16921168/scala-generic-method-no-classtag-available-for-t
        def score(state: State) : Double = {
            val i = state.i
            localScore(tags(i-1)(state.prev), tags(i)(state.cur), i)
        }
        val kBest : List[(List[T], Double)] =
            for { result <- decode_0th_order[KBest[Int]](tags.size, score _, (i: Int) => tags(i).size, KBest[Int](k) _).kbest
                } yield {
                    //logger(0, "tags.size = " + tags.size.toString)
                    //logger(0, "tag sizes = " + tags.map(x => x.size).toList)
                    //logger(0, "result.path = " + result.path)
                    val resultTags : List[T] = result.path.reverse.zipWithIndex.map(x => tags(x._2)(x._1))
                    //logger(0, "resultTags.size = " + resultTags.size.toString)
                    //logger(0, "resultTags = " + resultTags.toList)
                    (resultTags, result.score)
            }
        return kBest
    }

    private def decode_0th_order[SemiRingElem <: SemiRing[SemiRingElem] : ClassTag](length: Int,
                                     localScore: State => Double,
                                     tags: Int => Int,
                                     SemiRingElem: (Int, Double) => SemiRingElem) : SemiRingElem = {
        // 0th order Viterbi algorithm
        //   length: the length of the input (INCLUDING start and stop padding)
        //   localScore: State(prevState, curState, position) => transition weight (log prob)
        //   tags: position => number of tags
        //   SemiRingElem: constructs a semiring element from a tag (Int) and a score (Double)
        // returns the Viterbi semiring element assuming localScore is 0th order
        //logger(0, "length = " + length.toString)
        assert(length > 2, "Length must be greater than 2")
        assert(tags(0) == 1, "There must be a single start tag")
        assert(tags(length-1) == 1, "There must be a single stop tag")

        val T = length
        val viterbi = new Array[SemiRingElem](T)

        def max_cur(t: Int) : SemiRingElem = {
            // Find the most likely current state (highest model score)
            val sPrev : Int = 0 // doesn't matter, since localScore is 0th order
            val scores : List[SemiRingElem] = Range(0,tags(t)).map(sCur => SemiRingElem(sCur, localScore(State(sPrev,sCur,t))).times(viterbi(t-1))).toList
            return (scores.tail :\ scores.head)((a: SemiRingElem, b: SemiRingElem) => a.plus(b))
        }

        // Initialize (t = 0)
        viterbi(0) = SemiRingElem(0, 0.0)    // this is not the multiplicative identity (SemiRingOne)
        //logger(0, "viterbi(0) = "+viterbi(0).toList)
        //viterbi(0) = Array(SemiRingElem(0, 0.0))

        // Recursive
        for (t <- Range(1, T-1)) {
            viterbi(t) = max_cur(t)
            //logger(0, "viterbi("+t+") = "+viterbi(t).toList)
        }
        // Termination
        //logger(0, "viterbi("+(T-1).toString+") = "+max_prev(T-1, 0))

        return max_cur(T-1)
    }
}

