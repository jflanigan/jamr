package edu.cmu.lti.nlp.ner

import java.io.File
import java.io.FileOutputStream
import java.io.PrintStream
import java.io.BufferedOutputStream
import java.io.OutputStreamWriter
import java.lang.Math.abs
import java.lang.Math.log
import java.lang.Math.exp
import java.lang.Math.random
import java.lang.Math.floor
import java.lang.Math.min
import java.lang.Math.max
import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator._

/******************************** Decoder *******************************/

case class DecoderResult(tags: Array[Int], features: FeatureVector, score: Double)

class Decoder(_tagset: Array[String], feature_names: List[String]) {

    var weights : FeatureVector = FeatureVector()
    val features = new Features(feature_names)
    private val tag = _tagset  // note: if these are changed on the fly, then must change in features too

    def decode(unpaddedInput: Sentence) : DecoderResult = {
    // Viterbi algorithm modified from Fig 5.17 in Speech & Language Processing (p. 147)
    // Assume state 0 is START state, and state N - 1 is STOP state
    // (Assume input is not padded with a START token and a STOP token)
        val input = (Token("","",0) :: unpaddedInput.toList ::: List(Token("","",tag.size-1))).toArray
        val N = tag.size
        val T = input.size
        val viterbi = new Array[Array[Double]](T)
        val backpointers = new Array[Array[Int]](T)
        viterbi(1) = new Array[Double](N)
        backpointers(1) = new Array[Int](N)

        def max_prev(t: Int, sCur: Int) : (Double, Int) = {
            // Find the most likely previous state (highest model score)
            val scores = Range(1,N-1).map(sPrev => weights.dot(features.local(t, tag(sCur), tag(sPrev), input)))
            val (max, sPrev) = scores.zipWithIndex.maxBy(_._1)
            return (max, sPrev)
        }

        // Initialize (t = 1)
        for (s <- Range(1,N-1)) {
            viterbi(1)(s) = weights dot features.local(0, tag(s), tag(0), input)
            backpointers(1)(s) = 0
        }
        // Recursive
        for (t <- Range(2, T-1)) {
            viterbi(t) = new Array[Double](N)
            backpointers(t) = new Array[Int](N)
            for (s <- Range(1,N-1)) {
                val (max, sPrev) = max_prev(t, s)
                viterbi(t)(s) = max
                backpointers(t)(s) = sPrev
            }
        }
        // Termination
        val (max, sPrev) = max_prev(T-1, N-1)
        viterbi(T-1)(N-1) = max
        backpointers(T-1)(N-1) = sPrev
    
        // Follow backpointers
        val decode = new Array[Int](T)
        val feats = FeatureVector()
        var s = N - 1    // Final state
        for (t : Int <- Range(T-1, 0, -1)) {
            decode(t) = s
            feats += features.local(t, tag(s), tag(backpointers(t)(s)), input)
            s = backpointers(t)(s)
        }

        return DecoderResult(decode, feats, max) // TODO: change decode to tags, strip off <START> and <STOP>, also add taggedSentence member
    }

    def oracle(unpaddedInput: Sentence) : FeatureVector = {
        val input = (Token("","",0) :: unpaddedInput.toList ::: List(Token("","",tag.size-1))).toArray
        val N = tag.size
        val T = input.size

        val feats = FeatureVector()
        var s = N-1 // Final state
        for (t : Int <- Range(T-1, 0, -1)) {
            feats += features.local(t, tag(input(t).tag), tag(input(t-1).tag), input)
        }
        return feats
    }
}


