package edu.cmu.lti.nlp.amr.FastFeatureVector
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.Train._

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

class Adagrad extends Optimizer[FeatureVector] {
    def learnParameters(gradient: (Option[Int], Int, FeatureVector) => (FeatureVector, Double),
                        initialWeights: FeatureVector,
                        trainingSize: Int,
                        passes: Int,
                        stepsize: Double,
                        l2strength: Double,
                        noreg: List[String],
                        trainingObserver: (Int, FeatureVector) => Boolean,
                        avg: Boolean) : FeatureVector = {
        val weights = FeatureVector(initialWeights.labelset)
        weights += initialWeights
        var avg_weights = FeatureVector(weights.labelset)
        var sumSq = FeatureVector(weights.labelset)         // G_{i,i}
        var pass = 0
        while (pass < passes && (pass == 0 || trainingObserver(pass,avg_weights))) {
            logger(-1,"Pass "+(pass+1).toString)
            var objective = 0.0 // objective is 1/N \sum_i=1^N Loss(i) + 1/2 * \lambda * ||weights||^2 (var objective is N times this)
            for (t <- Random.shuffle(Range(0, trainingSize).toList)) {
                // normally we would do weights -= stepsize * gradient(t)._1
                // but instead we do this: (see equation 8 in SocherBauerManningNg_ACL2013.pdf)
                val (grad, score) = gradient(Some(pass), t, weights)
                sumSq.update(grad, (feat, label, x , y) => x + y * y)
                weights.update(grad, (feat, label, x, y) => {
                    val sq = sumSq(feat, label)
                    if (sq > 0.0) {
                        x - stepsize * y / sqrt(sumSq(feat, label))
                    } else {
                        x
                    }
                })
                objective += score
                if (l2strength != 0.0) {
                    val noregSaveValues = noreg.map(feat => (feat, weights.fmap(feat)))
                    noreg.map(feat => weights.fmap.remove(feat))
                    objective += weights.dot(weights) / 2.0   // don't count the unregularized features in the regularizer
                    sumSq.update(weights, (feat, label, x , y) => x + l2strength * l2strength * y * y)
                    weights.update(weights, (feat, label, x, y) => {
                        val sq = sumSq(feat, label)
                        if (sq > 0.0) {
                            x - stepsize * l2strength * y / sqrt(sumSq(feat, label))
                        } else {
                            x
                        }
                    })
                    noregSaveValues.map(x => { weights.fmap(x._1) = x._2 })
                }
            }
            logger(-1,"                                   Avg objective value last pass: "+(objective/trainingSize.toDouble).toString)
            //logger(0,"                                                       objective: "+((0 until trainingSize).map(x => gradient(None, x, weights)._2).sum/trainingSize).toString)
            avg_weights += weights
            pass += 1
        }
        trainingObserver(pass,avg_weights)
        if(avg) { avg_weights } else { weights }
    }
}

