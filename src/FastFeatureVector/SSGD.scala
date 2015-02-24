package edu.cmu.lti.nlp.amr.FastFeatureVector
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.Train._

import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.math.sqrt

class SSGD extends Optimizer[FeatureVector] {
    def learnParameters(gradient: (Option[Int], Int, FeatureVector) => (FeatureVector, Double),
                        initialWeights: FeatureVector,
                        trainingSize: Int,
                        noreg: List[String],
                        trainingObserver: (Int, FeatureVector) => Boolean,
                        options: Map[Symbol, String]) : FeatureVector = {
        val passes = options('trainingPasses).toInt
        val stepsize = options('trainingStepsize).toDouble
        val l2reg = options('trainingL2RegularizerStrength).toDouble
        val avg = options.contains('trainingAvgWeights)

        val weights = FeatureVector(initialWeights.labelset)
        weights += initialWeights
        var avg_weights = FeatureVector(weights.labelset)
        var i = 0
        var scaling_trick = 1.0
        while (i < passes && trainingObserver(i,weights)) {
            logger(0,"Pass "+(i+1).toString)
            var objective = 0.0 // objective is 1/N \sum_i=1^N Loss(i) + 1/2 * \lambda * ||weights||^2 (var objective is N times this)
            for (t <- Random.shuffle(Range(0, trainingSize).toList)) {
                // Usual update:
                //  weights -= stepsize * gradient(i, t)._1
                //  if (l2reg != 0.0) {
                //      weights -= (stepsize * l2reg) * weights
                //  }
                /************** Scaling trick ***************/
                //  true weights are scaling_trick * weights
                /********************************************/
                val (grad, score) = gradient(Some(i), t, weights)
                weights -= (stepsize / ((1.0-stepsize*l2reg)*scaling_trick)) * grad // no 2.0*stepsize*l2reg because we divide \lambda by two in our objective
                objective += score * scaling_trick
                for (feature <- noreg if weights.fmap.contains(feature)) {
                    val values = weights.fmap(feature)
                    objective += pow(values.unconjoined * scaling_trick, 2.0) / 2.0
                    objective += values.conjoined.map(x => pow(x._2 * scaling_trick, 2.0) / 2.0).sum
                    values.unconjoined /= (1.0 - stepsize * l2reg)  // so that value * scaling_trick = true weights after scaling_trick gets updated ( = value * scaling_trick(t) / scaling_trick(t+1) )
                    values.conjoined = values.conjoined.map(x => (x._1, x._2 / (1.0 - 2.0 * stepsize * l2reg)))
                }
                scaling_trick *= (1.0 - stepsize * l2reg)
            }
            // Undo scaling trick
            weights *= scaling_trick
            scaling_trick = 1.0
            logger(0,"                                                Avg objective value last pass: "+(objective/trainingSize.toDouble).toString)
            avg_weights += weights
            i += 1
        }
        trainingObserver(i,weights)
        avg_weights *= (1.0 / passes.toDouble)
        if(avg) { avg_weights } else { weights }
    }
}

