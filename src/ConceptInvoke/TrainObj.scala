package edu.cmu.lti.nlp.amr.ConceptInvoke
import edu.cmu.lti.nlp.amr._

import java.lang.Math.abs
import java.lang.Math.log
import java.lang.Math.exp
import java.lang.Math.random
import java.lang.Math.floor
import java.lang.Math.min
import java.lang.Math.max
import scala.io.Source
import scala.io.Source.stdin
import scala.io.Source.fromFile
import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

class TrainObj(val options : Map[Symbol, String]) extends edu.cmu.lti.nlp.amr.TrainObj(options) {

    val decoder = Decoder(options, oracle = false)
    val oracle = Decoder(options, oracle = true)
    val weights = decoder.features.weights
    oracle.features.weights = weights
    //costAugDecoder.features.weights = weights

    def decode(i: Int) : FeatureVector = {
        return decoder.decode(input(i)).features
    }

    def oracle(i: Int) : FeatureVector = {
        return oracle.decode(input(i)).features
    }

    def costAugmented(i: Int) : FeatureVector = {
        assert(false, "Need to implement stage1 cost augmented decoding")
        return decoder.decode(input(i)).features
    }
}

