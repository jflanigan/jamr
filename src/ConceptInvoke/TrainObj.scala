package edu.cmu.lti.nlp.amr.ConceptInvoke
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.Train._
import edu.cmu.lti.nlp.amr.BasicFeatureVector._

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

class TrainObj(val options : Map[Symbol, String]) extends edu.cmu.lti.nlp.amr.Train.TrainObj[FeatureVector](options) {

    // TODO: this implementation is not thread safe
    val decoder = Decoder(options, oracle = false)
    val oracle : Decoder = Decoder(options, oracle = true)
    //costAugDecoder.features.weights = weights

    var optimizer = options.getOrElse('trainingOptimizer, "Adagrad") match {     // TODO: this should go back into Train/TrainObj
        case "SSGD" => new SSGD()
        case "Adagrad" => new Adagrad()
        case x => { System.err.println("Error: unknown training optimizer " + x); sys.exit(1) }
    }

    def decode(i: Int, weights: FeatureVector) : (FeatureVector, Double, String) = {
        decoder.features.weights = weights
        val result = decoder.decode(input(i))
        return (result.features, result.score, "")
    }

    def oracle(i: Int, weights: FeatureVector) : (FeatureVector, Double) = {
        oracle.features.weights = weights
        val result = oracle.decode(input(i))
        return (result.features, result.score)
    }

    def costAugmented(i: Int, weights: FeatureVector, scale: Double) : (FeatureVector, Double) = {
        assert(false, "Need to implement stage1 cost augmented decoding")
        return (decoder.decode(input(i)).features, 0.0)
    }

    def train {
        train(FeatureVector())
    }

    def evalDev(options: Map[Symbol, String], pass: Int, weights: FeatureVector) { }

}

