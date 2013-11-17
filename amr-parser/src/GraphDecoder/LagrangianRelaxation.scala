package edu.cmu.lti.nlp.amr.GraphDecoder
import edu.cmu.lti.nlp.amr._

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
import Double.{NegativeInfinity => minusInfty}

class LagrangianRelaxation(featureNames: List[String], labelSet: Array[(String, Int)], stepsize: Double)
    extends Decoder(featureNames) {
    // Base class has defined:
    // val features: Features

    val alg2 = new Alg2("labelWithId" :: featureNames, labelSet)

    val labelConstraint = labelSet.toMap

    val multipliers = FeatureVector()
    val delta = 0.0
    def decode(input: Input) : DecoderResult = {
        features.input = input
        alg1.features.weights = features.weights    // Our weights same as alg2's (shared weights)

        var result = DecoderResult(Graph.empty(), FeatureVector(), 0.0)
        var delta = FeatureVector()
        do {
            //logger(2, "weights: \n"+features.weights)
            logger(1, "multipliers: \n"+multipliers.toString)
            features.weights -= multipliers
            //logger(2, "alg2 weights: \n"+features.weights)
            result = alg2.decode(input)
            logger(1, "features: \n"+result1.features.slice(x => IdFeature(x)))
            features.weights += multipliers // undo our adjustment to the weights

            delta = 0.0
            for ((feat, value) <- result.features.fmap if feat.startsWith("Id1=")) {
                val multiplier = multipliers.getOrElse(feat, 0.0)
                val newMultiplier = max(0.0, multiplier - stepsize * (labelConstraint(label) - feat))
                delta += abs(newMultiplier - multiplier)
                multipliers(feat) = newMultiplier
            }
        } while (delta != 0.0)

        val feats = result.features.slice(x => !x.startWith("Id1="))
        return DecoderResult(result.graph, feats, features.weights.dot(feats))
    }
}

