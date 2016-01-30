package edu.cmu.lti.nlp.amr.GraphDecoder
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.FastFeatureVector._

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
import java.lang.Math.sqrt
import scala.io.Source
import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import Double.{NegativeInfinity => minusInfty}

class LagrangianRelaxation(options: Map[Symbol, String], featureNames: List[String], labelSet: Array[(String, Int)])
        extends Decoder {
    // Base class has defined:
    // val features: Features
    val alg2 = new Alg2(options, "LRLabelWithId" :: featureNames, labelSet)
    options('stage2Decoder) = options.getOrElse('stage2ApproxDecoder, "Alg2")
    val approxDecoder = Decoder(options)
    options('stage2Decoder) = "LR"
    var features = alg2.features    // Set alg2 features same our features (so weights get updated during training, features are updated during cost augmented and LR)
    approxDecoder.features = alg2.features

    val labelConstraint = labelSet.toMap    // TODO: could change to array for speed

    val stepStrategy : String = options.getOrElse('stage2LRStepStrategy, "constant")
    val stepSize : Double = options.getOrElse('stage2LRStepSize, "1.0").toDouble
    val maxIterations : Int = options.getOrElse('stage2LRIterations, "500").toInt

    def decode(input: Input) : DecoderResult = {
        alg2.input = input      // (this also sets features.input)
        var result = DecoderResult(Graph.Null(), FeatureVector(features.weights.labelset), 0.0)

        val stepsizeMultiplier = if (stepStrategy == "adaptive" || stepStrategy == "adaptiveSqrtT") {
            val graph = input.graph.get.duplicate       // duplicate is unnecessary, just a precaution
            val nodes : List[Node] = graph.nodes.filter(_.name != None).toList
            val graphObj = new GraphObj(graph, nodes.toArray, alg2.features)
            max(graphObj.largestWeight, 1.0)    // we compute the largest weight before any Lagrange multipliers are set
        } else {
            1.0
        }

        val multipliers = FeatureVector(features.weights.labelset)
        var delta = 0.0         // so we know when we have converged
        var counter = 0
        do {
            //logger(2, "weights: \n"+features.weights)
            //logger(1, "multipliers: \n"+multipliers.toString)
            features.weights -= multipliers
            //logger(2, "alg2 weights: \n"+features.weights)
            result = alg2.decode
            //logger(1, "id features: \n"+result.features.slice(x => x.startsWith("LR:Id1=")))
            features.weights += multipliers // undo our adjustment to the weights

            delta = 0.0
            for { (feat, values) <- result.features.fmap
                  if feat.startsWith("LR:Id1=")
                  (labelIndex, value) <- values.conjoined
                } {
                //logger(1, "feat = "+feat)
                //logger(1, "labelIndex = "+labelIndex.toString)
                //logger(1, "value = "+value.toString)
                val multiplier : Double = multipliers(feat, Some(labelIndex))

                val stepsize : Double = stepStrategy match {
                    case "constant"          => stepSize
                    case "sqrtT"             => stepSize / sqrt(counter + 1)
                    case "adaptive"          => stepSize * stepsizeMultiplier
                    case "adaptiveSqrtT"     => stepSize * stepsizeMultiplier / sqrt(counter + 1)
                    case _                   => { assert(false, "Unknown stage2 LR step strategy: " + stepStrategy); 1.0 } 
                }

                val newMultiplier = max(0.0, multiplier - stepsize * (labelConstraint(features.weights.labelset(labelIndex)) - value))
                //logger(1, "newMultiplier = "+newMultiplier)
                delta += abs(newMultiplier - multiplier)
                multipliers.set((feat, Some(labelIndex)) -> newMultiplier)
            }

            /* ****** was (before FastFeatureVector): **********
            for ((feat, value) <- result.features.fmap if feat.startsWith("LR:Id1=")) {
                val multiplier = multipliers.fmap.getOrElse(feat, 0.0)
                val IdLabel = """LR:Id1.*[+]L=(.*)""".r
                val IdLabel(label) = feat
                val newMultiplier = max(0.0, multiplier - stepsize * (labelConstraint(label) - value))
                delta += abs(newMultiplier - multiplier)
                multipliers.fmap(feat) = newMultiplier
            } ************************************************* */

            counter += 1
        } while (delta != 0.0 && counter < maxIterations)

        if (delta != 0.0) {
            logger(0, "WARNING: Lagrangian relaxation did not converge after "+counter.toString+" iterations. Delta = "+delta.toString)
            if (options.contains('stage2ApproxDecoder)) {
                logger(0, "Running approximate decoder " + options('stage2ApproxDecoder))
                result = approxDecoder.decode(input)
            }
        } else {
            logger(0, "Lagrangian relaxation converged after "+counter.toString+" iterations. Delta = "+delta.toString)
        }

        val feats = result.features.filter(x => !x.startsWith("LR:Id1="))
        return DecoderResult(result.graph, feats, features.weights.dot(feats))
    }
}

