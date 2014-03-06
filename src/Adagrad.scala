package edu.cmu.lti.nlp.amr

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
import scala.util.Random
import scala.math.sqrt

/******************************** Training **********************************/

class Adagrad extends Optimizer {
    def learnParameters(gradient: (Int, Int) => FeatureVector,
                        weights: FeatureVector,
                        trainingSize: Int,
                        passes: Int,
                        stepsize: Double,
                        avg: Boolean) : FeatureVector = {
        var avg_weights = FeatureVector()
        var sumSq = FeatureVector()         // G_{i,i}
        for (pass <- Range(0,passes)) {
            logger(0,"Pass "+(pass+1).toString)
            for (t <- Random.shuffle(Range(0, trainingSize).toList)) {
            //for (t <- Range(0, trainingSize)) {
                // normally we would do weights -= stepsize * gradient(t)
                // but instead we do this: (see equation 8 in SocherBauerManningNg_ACL2013.pdf)
                for ((feat, value) <- gradient(pass, t).fmap
                     if value != 0.0 ) {
                    sumSq.fmap(feat) = sumSq.fmap.getOrElse(feat, 0.0) + value * value
                    weights.fmap(feat) = weights.fmap.getOrElse(feat, 0.0) - stepsize * value / sqrt(sumSq.fmap(feat))
                }
            }
            avg_weights += weights
        }
        if(avg) { avg_weights } else { weights }
    }
}

