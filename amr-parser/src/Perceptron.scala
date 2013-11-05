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

/******************************** Training **********************************/

object Perceptron {
    def learnParameters(decoder: Int => FeatureVector,
                        oracle: Int => FeatureVector,
                        weights: FeatureVector,
                        trainingSize: Int,
                        passes: Int,
                        avg: Boolean) : FeatureVector = {
        var avg_weights = FeatureVector()
        val permutations = Range(0, trainingSize).permutations
        var corpus = permutations.next
        for (i <- Range(1,passes+1)) {
            logger(0,"Pass "+i.toString)
            if (permutations.hasNext) {
                corpus = permutations.next
            }
            for (t <- Range(0, trainingSize)) {
                logger(1,"-- Weights --")
                logger(1,weights)
//                weights -= decoder.decode(example).features
//                weights += decoder.oracle(example)
                val minus = decoder(t)
                val plus = oracle(t)
                weights -= minus
                weights += plus
                plus -= minus
                logger(1,"-- Difference --")
                logger(1,plus)
            }
            avg_weights += weights
        }
        if(avg) { avg_weights } else { weights }
    }
}

