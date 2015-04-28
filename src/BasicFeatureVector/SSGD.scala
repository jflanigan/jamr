package edu.cmu.lti.nlp.amr.BasicFeatureVector
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.Train._

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

class SSGD extends Optimizer[FeatureVector] {
    def learnParameters(gradient: (Option[Int], Int, FeatureVector) => (FeatureVector, Double),
                        weights: FeatureVector,
                        trainingSize: Int,
                        noreg: List[String],  // TODO: implement
                        trainingObserver: (Int, FeatureVector) => Boolean,
                        options: Map[Symbol, String]) : FeatureVector = {
        val passes = options('trainingPasses).toInt
        val stepsize = options('trainingStepsize).toDouble
        val l2reg = options('trainingL2RegularizerStrength).toDouble
        val avg = options.contains('trainingAvgWeights)

        var avg_weights = FeatureVector()
        var i = 0
        while (i < passes && trainingObserver(i, weights)) {
            logger(0,"Pass "+(i+1).toString)
            for (t <- Random.shuffle(Range(0, trainingSize).toList)) {
                weights -= stepsize * gradient(Some(i), t, weights)._1
                if (l2reg != 0.0) {
                    weights -= (stepsize * l2reg) * weights
                }
            }
            avg_weights += weights
            i += 1
        }
        trainingObserver(i, weights)
        if(avg) { avg_weights } else { weights }
    }
}

