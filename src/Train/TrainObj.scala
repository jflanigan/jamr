package edu.cmu.lti.nlp.amr.Train
import edu.cmu.lti.nlp.amr._

import java.lang.Math.abs
import java.lang.Math.log
import java.lang.Math.exp
import java.lang.Math.random
import java.lang.Math.floor
import java.lang.Math.min
import java.lang.Math.max
import java.io.StringWriter
import java.io.PrintWriter
import scala.io.Source
import scala.io.Source.stdin
import scala.io.Source.fromFile
import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

abstract class TrainObj[FeatureVector <: AbstractFeatureVector](options: Map[Symbol, String])  {

    def decode(i: Int, weights: FeatureVector) : (FeatureVector, Double, String)
    def oracle(i: Int, weights: FeatureVector) : (FeatureVector, Double)
    def costAugmented(i: Int, weights: FeatureVector, scale: Double) : (FeatureVector, Double)
    def train : Unit
    def evalDev(options: Map[Symbol, String], pass: Int, weights: FeatureVector) : Unit
    def zeroVector : FeatureVector

    ////////////////// Training Setup ////////////////

    val passes = options.getOrElse('trainingPasses, "20").toInt
    val stepsize = options.getOrElse('trainingStepsize, "1.0").toDouble
    val l2RegularizerStrength = options.getOrElse('trainingL2RegularizerStrength, "0.0").toDouble
    val loss = options.getOrElse('trainingLoss, "Perceptron")
    if (options.contains('trainingSaveInterval) && !options.contains('trainingOutputFile)) {
        System.err.println("Error: trainingSaveInterval specified but output weights filename given"); sys.exit(1)
    }

    var optimizer: Optimizer[FeatureVector]

    val numThreads = options.getOrElse('numThreads,"4").toInt
    if (options.getOrElse('trainingMiniBatchSize,"1").toInt > 1) {
        optimizer = new MiniBatch(optimizer, options('trainingMiniBatchSize).toInt, numThreads)
    }

    val input = Input.loadInputfiles(options)
    val training: Array[String] = Corpus.getAmrBlocks(io.Source.stdin.getLines()).toArray

/*  Runtime.getRuntime().addShutdownHook(new Thread() {
        override def run() {
            System.err.print("Writing out weights... ")
            if (options.contains('trainingWeightsFile)) {
                val file = new java.io.PrintWriter(new java.io.File(options('trainingWeightsFile)), "UTF-8")
                try { file.print(weights.toString) }
                finally { file.close }
            } else {
                print(weights.unsorted)
            }
            System.err.println("done")
        }
    }) */

    /////////////////////////////////////////////////

    def gradient(i: Int, weights: FeatureVector) : (FeatureVector, Double) = {
        val scale = options.getOrElse('trainingCostScale,"1.0").toDouble
        try {
            if (loss == "Perceptron") {
                val (grad, score, _) = decode(i, weights)
                val o = oracle(i, weights)
                grad -= o._1
                //logger(0, "Gradient:\n"+grad.toString)
                (grad, score - o._2)
            } else if (loss == "SVM") {
                val (grad, score) = costAugmented(i, weights, scale)
                val o = oracle(i, weights)
                grad -= o._1
                (grad, score - o._2)
            } else if (loss == "Ramp1") {
                val (grad, score) = costAugmented(i, weights, scale)
                val o = decode(i, weights)
                grad -= o._1
                (grad, score - o._2)
            } else if (loss == "Ramp2") {
                val (grad, score, _) = decode(i, weights)
                val o = costAugmented(i, weights, -1.0 * scale)
                grad -= o._1
                (grad, score - o._2)
            } else if (loss == "Ramp3") {
                val (grad, score) = costAugmented(i, weights, scale)
                val o = costAugmented(i, weights, -1.0 * scale)
                grad -= o._1
                (grad, score - o._2)
            } else {
                System.err.println("Error: unknown training loss " + loss); sys.exit(1).asInstanceOf[Nothing]
            }
        } catch {
            case e : Throwable => if (options.contains('ignoreParserErrors)) {
                logger(-1, " ********** THERE WAS AN EXCEPTION IN THE PARSER. *********")
                if (verbosity >= -1) { e.printStackTrace }
                logger(-1, "Continuing. To exit on errors, please run without --ignore-parser-errors")
                (zeroVector , 0.0)
            } else {
                throw e
            }
        }
    }

    def trainingObserver(pass: Int, weights: FeatureVector) : Boolean = {
        if (options.contains('trainingSaveInterval) && pass % options('trainingSaveInterval).toInt == 0 && pass > 0) {
            val file = new java.io.PrintWriter(new java.io.File(options('trainingOutputFile) + ".iter" + pass.toString), "UTF-8")
            try { file.print(weights.toString) }
            finally { file.close }
        }
        evalDev(options, pass, weights)
        return true
    }

/*  TODO: Add eval on dev dataset
    def evalDev(pass: Int, weights: FeatureVector) {   // TODO: doesn't account for regularizer
        val formalism = options('formalism)
        val devfile = "data/splits/sec20."+formalism+".sdp"
        val (iASSave, iGSave, oGSave) = (inputAnnotatedSentences, inputGraphs, oracleGraphs)
        inputAnnotatedSentences = Corpus.getInputAnnotatedSentences(devfile+".dependencies")
        inputGraphs = Corpus.splitOnNewline(fromFile(devfile, "utf-8").getLines).map(
            x => SDPGraph.fromGold(x.split("\n"), true)).toArray
        oracleGraphs = Corpus.splitOnNewline(fromFile(devfile, "utf-8").getLines).map(
            x => SDPGraph.fromGold(x.split("\n"), false)).toArray
        assert(inputAnnotatedSentences.size == inputGraphs.size && inputGraphs.size == oracleGraphs.size, "sdp and dep file lengths do not match")

        //val numThreads = options.getOrElse('numThreads,"1").toInt
        val par = Range(0, inputAnnotatedSentences.size).par
        par.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(numThreads)) // TODO: use numThread option
        val loss = par.map(i => gradient(i, weights)).reduce((a, b) => ({ a._1 += b._1; a._1 }, a._2 + b._2))._2 / inputAnnotatedSentences.size.toDouble
        logger(0, "Dev loss: " + loss.toString)

        val predictions : String = par.map(i => decode(i, weights)._3).seq.mkString("\n\n")
        val file = new java.io.PrintWriter(new java.io.File(options('model) + ".iter" + pass.toString+".preds"), "UTF-8")
        try { file.print(predictions) }
        finally { file.close }
        //logger(0, "Command: /home/jmflanig/work/semeval-2014_HOLS/scripts/eval.sh data/splits/sec20."+formalism+".sdp " + options('model) + ".iter" + pass.toString+".preds")
        try {
        val externalEval = stringToProcess("/home/jmflanig/work/semeval-2014_feats/scripts/eval.sh data/splits/sec20."+formalism+".sdp " + options('model) + ".iter" + pass.toString+".preds").lines.toList
        if (externalEval.size == 46) {
            logger(0, "--- Performance on Dev ---\n" + externalEval.slice(14,18).mkString("\n") + "\n")
        } else {
            logger(0, "--- Performance on Dev ---\n" + externalEval.mkString("\n") + "\n")
        }
        } catch {
            case _ : Throwable => 
        }

        inputAnnotatedSentences = iASSave
        inputGraphs = iGSave
        oracleGraphs = oGSave
    } */

    def train(initialWeights: FeatureVector) {
        val weights = optimizer.learnParameters(
            (i,w) => gradient(i,w),
            initialWeights,
            input.size,
            passes,
            stepsize,
            options.getOrElse('trainingL2RegularizerStrength, "0.0").toDouble,
            List("Bias"),   // don't regularize the bias terms
            trainingObserver,
            avg = options.contains('trainingAvgWeights))

        System.err.print("Writing out weights... ")
        if (options.contains('trainingWeightsFile)) {
            val file = new java.io.PrintWriter(new java.io.File(options('trainingWeightsFile)), "UTF-8")
            try { file.print(weights.toString) }
            finally { file.close }
        } else {
            print(weights.unsorted)
        }
        System.err.println("done")
    }
}

