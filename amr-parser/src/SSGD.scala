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

class SSGD extends Optimizer {
    def learnParameters(gradient: Int => FeatureVector,
                        weights: FeatureVector,
                        trainingSize: Int,
                        passes: Int,
                        stepsize: Double,
                        avg: Boolean) : FeatureVector = {
        var avg_weights = FeatureVector()
        for (i <- Range(1,passes+1)) {
            logger(0,"Pass "+i.toString)
            for (t <- Random.shuffle(Range(0, trainingSize).toList)) {
                weights -= stepsize * gradient(t)
            }
            avg_weights += weights
        }
        if(avg) { avg_weights } else { weights }
    }
}

