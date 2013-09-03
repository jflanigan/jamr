package edu.cmu.lti.nlp.ner

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

class Perceptron {

def learnParameters(decoder: Decoder,
                    trainingFile: String,
                    passes: Int,
                    avg: Boolean,
                    features: Features) : FeatureVector = {
    var weights = FeatureVector()
    var avg_weights = FeatureVector()
    val training = new TrainingData(trainingFile)
    val tagset = training.tagset
    val permutations = training.corpus.permutations
    for (i <- Range(1,passes+1)) {
        val corpus = permutations.next
        for (example <- corpus) {
            weights -= decoder.decode(example).features
            weights += decoder.oracle(example)
        }
        avg_weights += weights
    }
    if(avg) { avg_weights } else { weights }
}

}


