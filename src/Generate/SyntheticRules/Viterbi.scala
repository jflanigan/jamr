package edu.cmu.lti.nlp.amr.Generate.SyntheticRules
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

import scala.reflect.ClassTag

/******************************** Viterbi Decoder *******************************/

object Viterbi {

    def decode[T:ClassTag](tags: Array[Array[T]], localScore: (T,T,Int) => Double) : (List[T], Double) = { // for why ClassTag is needed, see http://stackoverflow.com/questions/16921168/scala-generic-method-no-classtag-available-for-t
        def score(state: State) : Double = {
            val i = state.i
            localScore(tags(i-1)(state.prev), tags(i)(state.cur), i)
        }
        val result = decode(tags.size, score, i => tags(i).size)
        val resultTags : List[T] = result.tagseq.zipWithIndex.map(x => tags(x._2)(x._1)).toList
        return (resultTags, result.score)
    }

    private case class State(prev: Int, cur: Int, i: Int)
    private case class DecoderResult(tagseq: Array[Int], score: Double)

    private def decode(length: Int, localScore: State => Double, tags: Int => Int) : DecoderResult = {
        // Viterbi algorithm modified from Fig 5.17 in Speech & Language Processing (p. 147)
        //   length: the length of the input (INCLUDING start and stop padding)
        //   localScore: State(prevState, curState, position) => transition weight (log prob)
        //   tags: position => number of tags
        // returns (tag_sequence, score) where tag_sequence.size = length
        assert(length > 2, "Length must be greater than 2")
        assert(tags(0) == 1, "There must be a single start tag")
        assert(tags(length-1) == 1, "There must be a single stop tag")

        val T = length
        val viterbi = new Array[Array[Double]](T)
        val backpointers = new Array[Array[Int]](T)

        def max_prev(t: Int, sCur: Int) : (Double, Int) = {
            // Find the most likely previous state (highest model score)
            val scores = Range(0,tags(t-1)).map(sPrev => localScore(State(sPrev,sCur,t)) + viterbi(t-1)(sPrev))
            val (max, sPrev) = scores.zipWithIndex.maxBy(_._1)
            return (max, sPrev)
        }

        // Initialize (t = 1)
        viterbi(1) = new Array[Double](tags(1))
        backpointers(1) = new Array[Int](tags(1))
        for (s <- Range(0, tags(1))) {
            viterbi(1)(s) = localScore(State(0, s, 1))
            backpointers(1)(s) = 0
        }
        // Recursive
        for (t <- Range(2, T-1)) {
            viterbi(t) = new Array[Double](tags(t))
            backpointers(t) = new Array[Int](tags(t))
            for (s <- Range(0, tags(t))) {
                val (max, sPrev) = max_prev(t, s)
                viterbi(t)(s) = max
                backpointers(t)(s) = sPrev
            }
        }
        // Termination
        val (max, sPrev) = max_prev(T-1, 0)
        viterbi(T-1) = new Array[Double](1)
        viterbi(T-1)(0) = max
        backpointers(T-1) = new Array[Int](1)
        backpointers(T-1)(0) = sPrev
    
        // Follow backpointers
        logger(3,"-- Viterbi --")  // CHANGE
        val decode = new Array[Int](T)
        var s = 0       // Final state
        for (t : Int <- Range(T-1, 0, -1)) {
            decode(t) = s
            if(verbosity >= 3) {
                logger(3,"Viterbi("+t+") = "+viterbi(t).toList.toString)
                logger(3,"backpointers("+t+") = "+backpointers(t).toList.toString)
            }
            s = backpointers(t)(s)
        }
        decode(0) = 0

        return DecoderResult(decode, max)
    }

}

