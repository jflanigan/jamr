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

class LagrangianRelaxation(featureNames: List[String], labelSet: Array[(String, Int)], stepsize: Double, maxIterations: Int)
        extends Decoder {
    // Base class has defined:
    // val features: Features
    val features = new Features(featureNames)
    val alg2 = new Alg2("LRLabelWithId" :: featureNames, labelSet)
    alg2.features.weights = features.weights    // Set alg2's weights same our weights (shared weights)

    val labelConstraint = labelSet.toMap
    val IdLabel = """LR:Id1.*[+]L=(.*)""".r

    def decode(input: Input) : DecoderResult = {
        features.input = input
        alg2.input = input
        var result = DecoderResult(Graph.empty(), FeatureVector(), 0.0)

        val multipliers = FeatureVector()
        var delta = 0.0         // so we know when we have converged
        var counter = 0
        do {
            //logger(2, "weights: \n"+features.weights)
            logger(1, "multipliers: \n"+multipliers.toString)
            features.weights -= multipliers
            //logger(2, "alg2 weights: \n"+features.weights)
            result = alg2.decode
            logger(1, "id features: \n"+result.features.slice(x => x.startsWith("LR:Id1=")))
            features.weights += multipliers // undo our adjustment to the weights

            delta = 0.0
            for ((feat, value) <- result.features.fmap if feat.startsWith("LR:Id1=")) {
                val multiplier = multipliers.fmap.getOrElse(feat, 0.0)
                val IdLabel(label) = feat
                val newMultiplier = max(0.0, multiplier - stepsize * (labelConstraint(label) - value))
                delta += abs(newMultiplier - multiplier)
                multipliers.fmap(feat) = newMultiplier
            }
            counter += 1
        } while (delta != 0.0 && counter < maxIterations)

        if (delta != 0.0) {
            logger(0, "WARNING: Langrangian relaxation did not converge after "+counter.toString+" iterations. Delta = "+delta.toString)
        } else {
            logger(0, "Langrangian relaxation converged after "+counter.toString+" iterations. Delta = "+delta.toString)
        }

        val feats = result.features.slice(x => !x.startsWith("LR:Id1="))
        return DecoderResult(result.graph, feats, features.weights.dot(feats))
    }
}

