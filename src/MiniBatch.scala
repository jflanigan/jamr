package edu.cmu.lti.nlp.amr

import java.lang.Math.abs
import java.lang.Math.log
import java.lang.Math.exp
import java.lang.Math.random
import java.lang.Math.floor
import java.lang.Math.ceil
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

class MiniBatch(optimizer: Optimizer, miniBatchSize: Int) extends Optimizer {
    def learnParameters(gradient: (Int, Int) => FeatureVector,
                        weights: FeatureVector,
                        trainingSize: Int,
                        passes: Int,
                        stepsize: Double,
                        l2reg: Double,
                        trainingObserver: Int => Boolean,
                        avg: Boolean) : FeatureVector = {
        val numMiniBatches = ceil(trainingSize.toDouble / miniBatchSize.toDouble).toInt
        val trainShuffle : Array[Array[Int]] = Range(0, passes).map(x => Random.shuffle(Range(0, trainingSize).toList).toArray).toArray
        val miniGradient : (Int, Int) => FeatureVector = (pass, i) => {
            //var grad = FeatureVector()
            assert(i < numMiniBatches, "MiniBatch optimizer mini-batch index too large")
            //for (j <- Range(i*miniBatchSize, min((i+1)*miniBatchSize, trainingSize))) {
            //    grad += gradient(0, trainShuffle(pass)(j))
            //}
            //return grad
            val par = Range(i*miniBatchSize, min((i+1)*miniBatchSize, trainingSize)).par
            val grad = par.map(x => gradient(0, trainShuffle(pass)(x))).seq // TODO: if FeatureVector was immutable, wouldn't need to do convert to non-parallel collection...
            return grad.reduce((a, b) => { a += b; a })
        }
        return optimizer.learnParameters(miniGradient,
                                         weights,
                                         numMiniBatches,
                                         passes,
                                         stepsize / miniBatchSize.toDouble,
                                         l2reg * miniBatchSize,
                                         trainingObserver,
                                         avg)
    }
}

