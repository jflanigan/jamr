package edu.cmu.lti.nlp.amr.Train

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

abstract class Optimizer[FeatureVector <: AbstractFeatureVector] {
    def learnParameters(gradient: (Int, FeatureVector) => (FeatureVector, Double),
                        initialWeights: FeatureVector,
                        trainingSize: Int,
                        passes: Int,
                        stepsize: Double,
                        l2reg: Double,
                        noreg: List[String],
                        avg: Boolean) : FeatureVector = {
        val myGrad : (Option[Int], Int, FeatureVector) => (FeatureVector, Double) = (pass, i, w) => gradient(i,w)
        return learnParameters(myGrad, initialWeights, trainingSize, passes, stepsize, l2reg, noreg, (x: Int, w: FeatureVector) => true, avg)
    }

    def learnParameters(gradient: (Int, FeatureVector) => (FeatureVector, Double),
                        initialWeights: FeatureVector,
                        trainingSize: Int,
                        passes: Int,
                        stepsize: Double,
                        l2reg: Double,
                        noreg: List[String],
                        trainingObserver: (Int, FeatureVector) => Boolean,
                        avg: Boolean) : FeatureVector = {
        val myGrad : (Option[Int], Int, FeatureVector) => (FeatureVector, Double) = (pass, i, w) => gradient(i,w)
        return learnParameters(myGrad, initialWeights, trainingSize, passes, stepsize, l2reg, noreg, trainingObserver, avg)
    }

    def learnParameters(gradient: (Option[Int], Int, FeatureVector) => (FeatureVector, Double),              // Input: (pass, i, weights) Output: (gradient, objective value)
                        initialWeights: FeatureVector,
                        trainingSize: Int,
                        passes: Int,
                        stepsize: Double,
                        l2reg: Double,
                        noreg: List[String],
                        trainingObserver: (Int, FeatureVector) => Boolean,  // Input: pass, weights  Output: true stops training loop
                        avg: Boolean) : FeatureVector

}

