package edu.cmu.lti.nlp.amr

import java.lang.Math.abs
import java.lang.Math.log
import java.lang.Math.exp
import java.lang.Math.random
import java.lang.Math.floor
import java.lang.Math.min
import java.lang.Math.max
import scala.io.Source
import scala.io.Source.stdin
import scala.io.Source.fromFile
import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

abstract class TrainObj(options: Map[Symbol, String])  {

    def decode(i: Int) : FeatureVector
    def oracle(i: Int) : FeatureVector
    def costAugmented(i: Int) : FeatureVector
    val weights : FeatureVector

    ////////////////// Training Setup ////////////////

    val passes = options.getOrElse('trainingPasses, "20").toInt
    val stepsize = options.getOrElse('trainingStepsize, "1.0").toDouble
    val regularizerStrength = options.getOrElse('trainingRegularizerStrength, "0.0").toDouble
    if (!options.contains('trainingLoss)) {
        System.err.println("Error: No training loss specified"); sys.exit(1)
    }
    val loss = options('trainingLoss)
    if (!options.contains('trainingOptimizer)) {
        System.err.println("Error: No training optimizer specified"); sys.exit(1)
    }
    val optimizer: Optimizer = options('trainingOptimizer).asInstanceOf[String] match {
        case "SSGD" => new SSGD()
        case "Adagrad" => new Adagrad()
        case x => { System.err.println("Error: unknown training optimizer " + x); sys.exit(1) }
    }

    val input = Input.loadInputfiles(options)

    val training: Array[String] = (for {
        block <- Corpus.splitOnNewline(io.Source.stdin.getLines())
        if block.matches("(.|\n)*\n\\((.|\n)*")     // needs to contain some AMR
    } yield block).toArray

    Runtime.getRuntime().addShutdownHook(new Thread() {
        override def run() {
            System.err.print("Writing out weights... ")
            print(weights.unsorted)
            System.err.println("done")
        }
    })

    /////////////////////////////////////////////////

    def gradient(i: Int) : FeatureVector = {
        if (loss == "Perceptron") {
            val grad = decode(i)
            grad -= oracle(i)
            grad
        } else if (loss == "SVM") {
            val grad = costAugmented(i)
            grad -= oracle(i)
            grad
        } else {
            System.err.println("Error: unknown training loss " + loss); sys.exit(1)
            FeatureVector()
        }
    }

    def train() {
        optimizer.learnParameters(
            i => gradient(i),
            weights,
            training.size,
            passes,
            stepsize,
            avg = false)
    }
}

