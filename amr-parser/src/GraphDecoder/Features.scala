package edu.cmu.lti.nlp.amr.GraphDecoder
import edu.cmu.lti.nlp.amr._

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

/**************************** Feature Functions *****************************/

class Features(featureNames: List[String]) {
    var weights = FeatureVector()
    type FeatureFunction = (Node, Node, String, Input) => FeatureVector
    type RootFeatureFunction = (Node, Input) => FeatureVector

    val ffTable = Map[String, FeatureFunction](
        "edgeId" -> ffEdgeId,
        "bias" -> ffBias,
        "conceptBigram" -> ffConceptBigram
    )

    val rootFFTable = Map[String, RootFeatureFunction](
    )

    val rootFeature = List("rootPath")
    val notFast = List()  // ff that don't support fast lookup

/*    var prev_t : Int = -1       // For fast features
    var prev_input = Array[Token]()
    var prev_sPrev = ""
    var saved = FeatureVector() */

    var featureFunctions : List[FeatureFunction] = {
        for { feature <- featureNames
              if !rootFeature.contains(feature)
              if !notFast.contains(feature)
        } yield ffTable(feature)
    } // TODO: error checking on lookup

/*    val featureFunctionsNotFast : List[FeatureFunction] = {
        for { feature <- feature_names
              if nofast.contains(feature)
        } yield fftable(feature)
    }
    //logger(0,feature_names)
*/

    var rootFeatureFunctions : List[RootFeatureFunction] = {
        for { feature <- featureNames
              if rootFeature.contains(feature)
        } yield rootFFTable(feature)
    } // TODO: error checking on lookup


    def setFeatures(featureNames: List[String]) {
        featureFunctions = featureNames.filter(x => !notFast.contains(x)).map(x => ffTable(x))
        //featureFunctionsNotFast = featureNames.filter(x => notFast.contains(x)).map(x => ffTable(x))
    }

    def localFeatures(node1: Node, node2: Node, label: String, input: Input) : FeatureVector = {
        // Calculate the local features
        val feats = FeatureVector()
        for (ff <- featureFunctions) {
            feats += ff(node1, node2, label, input)
        }
        return feats
    }

    def localScore(node1: Node, node2: Node, label: String, input: Input) : Double = {
        var score = 0.0
        for (ff <- featureFunctions) {
            logger(2, ff.toString)
            logger(2, ff(node1, node2, label, input))
            score += weights.dot(ff(node1, node2, label, input))
        }
        return score
    }

    def rootFeatures(node: Node, input: Input) : FeatureVector = {
        // Calculate the local features
        val feats = FeatureVector()
        for (ff <- rootFeatureFunctions) {
            feats += ff(node, input)
        }
        return feats
    }

    def rootScore(node: Node, input: Input) : Double = {
        var score = 0.0
        for (ff <- rootFeatureFunctions) {
            logger(2, ff.toString)
            logger(2, ff(node, input))
            score += weights.dot(ff(node, input))
        }
        return score
    }

    // node1 is always the tail, and node2 the head

    def ffEdgeId(node1: Node, node2: Node, label: String, input: Input) : FeatureVector = {  
        return FeatureVector(Map(("Id1="+node1.id+":Id2="+node2.id+":L="+label) -> 1.0))
    }

    def ffBias(node1: Node, node2: Node, label: String, input: Input) : FeatureVector = {
        return FeatureVector(Map(("L="+label) -> 1.0))
    }

    def ffConceptBigram(node1: Node, node2: Node, label: String, input: Input) : FeatureVector = {
        return FeatureVector(Map(/*("C1="+node1.concept+":C2="+node2.concept) -> 1.0,*/
                                 ("C1="+node1.concept+":C2="+node2.concept+":L="+label) -> 1.0))
    }
}

