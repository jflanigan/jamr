package edu.cmu.lti.nlp.amr

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
import scala.util.Random
import scala.math.sqrt

/******************************** Training **********************************/

object Adagrad {
    def learnParameters(decoder: Int => FeatureVector,
                        oracle: Int => FeatureVector,
                        weights: FeatureVector,
                        trainingSize: Int,
                        passes: Int,
                        stepsize: Double,
                        avg: Boolean) : FeatureVector = {
        var avg_weights = FeatureVector()
        var sumSq = FeatureVector()         // G_{i,i}
        for (i <- Range(1,passes+1)) {
            logger(0,"Pass "+i.toString)
            for (t <- Random.shuffle(Range(0, trainingSize))) {
//                logger(1,"-- Weights --")
//                logger(1,weights)
//                weights -= decoder.decode(example).features
//                weights += decoder.oracle(example)
                val minus = decoder(t)
                val plus = oracle(t)
                logger(1,"-- Good --")
                logger(1,plus)
                logger(1,"-- Bad --")
                logger(1,minus)
                plus -= minus
                logger(1,"-- Difference --")
                logger(1,plus)
                // -gradient is in 'plus'
                // normally we would do weights += stepsize * plus
                // but instead we do this: (see equation 8 in SocherBauerManningNg_ACL2013.pdf)
                for ((feat, value) <- plus) {
                    sumSq(feat) = sumSq(feat).getOrElse(feat, 0.0) + value * value
                    weights(feat) = weights(feat).getOrElse(feat, 0.0) + stepsize * value / sqrt(sumSq(feat))
                }
            }
            avg_weights += weights
        }
        if(avg) { avg_weights } else { weights }
    }
}

