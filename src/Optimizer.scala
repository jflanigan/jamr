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
import scala.util.parsing.combinator._
import scala.util.Random
import scala.math.sqrt

abstract class Optimizer {
    def learnParameters(gradient: Int => FeatureVector,
                        weights: FeatureVector,
                        trainingSize: Int,
                        passes: Int,
                        stepsize: Double,
                        avg: Boolean) : FeatureVector = {
        val myGrad : (Int, Int) => FeatureVector = (pass, i) => gradient(i)
        return learnParameters(myGrad, weights, trainingSize, passes, stepsize, (x: Int) => true, avg)
    }

    def learnParameters(gradient: Int => FeatureVector,
                        weights: FeatureVector,
                        trainingSize: Int,
                        passes: Int,
                        stepsize: Double,
                        trainingObserver: Int => Boolean,
                        avg: Boolean) : FeatureVector = {
        val myGrad : (Int, Int) => FeatureVector = (pass, i) => gradient(i)
        return learnParameters(myGrad, weights, trainingSize, passes, stepsize, trainingObserver, avg)
    }

    def learnParameters(gradient: (Int, Int) => FeatureVector,      // Input: (pass, i) Output: gradient
                        weights: FeatureVector,
                        trainingSize: Int,
                        passes: Int,
                        stepsize: Double,
                        trainingObserver: Int => Boolean,    // Input: pass  Output: false stops training loop
                        avg: Boolean) : FeatureVector
}

