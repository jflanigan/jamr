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
    private var graph: Graph = _
    private var sentence: Array[String] = _
    private var dependencies: Annotation[Array[Dependency]] = _
    private var pos: Annotation[Array[String]] = _

    def input: Input = Input(graph, sentence, dependencies, pos)
    def input_= (i: Input) {
        graph = i.graph
        sentence = i.sentence
        dependencies = i.dependencies
        pos = i.pos
        precompute
    }

    type FeatureFunction = (Node, Node, String) => FeatureVector
    type RootFeatureFunction = (Node) => FeatureVector

    val ffTable = Map[String, FeatureFunction](
        "edgeId" -> ffEdgeId,
        "bias" -> ffBias,
        "conceptBigram" -> ffConceptBigram,
        "dependencyPath" -> ffDependencyPath
    )

    val rootFFTable = Map[String, RootFeatureFunction](
        "rootConcept" -> ffRootConcept
    )

    def precompute() {
        if (featureNames.contains("dependencyPath")) {
            rootDependencyPaths = dependencies.tok.indices.map(i => rootDependencyPath(i).reverse).toArray
        }
    }

    // node1 is always the tail, and node2 the head

    def ffEdgeId(node1: Node, node2: Node, label: String) : FeatureVector = {  
        return FeatureVector(Map(("Id1="+node1.id+"+Id2="+node2.id+"+L="+label) -> 1.0))
    }

    def ffBias(node1: Node, node2: Node, label: String) : FeatureVector = {
        return FeatureVector(Map(("L="+label) -> 1.0))
    }

    def ffConceptBigram(node1: Node, node2: Node, label: String) : FeatureVector = {
        //logger(2, "ffConceptBigram: Node1 = " + node1.concept + " Node2 = " + node2.concept + " label = " + label)
        return FeatureVector(Map(/*("C1="+node1.concept+":C2="+node2.concept) -> 1.0,*/
                                 ("C1="+node1.concept+"+C2="+node2.concept+"+L="+label) -> 1.0))
    }

    var rootDependencyPaths : Array[List[Int]] = _

    def dependencySpan(node: Node) : Range = {
        val span = dependencies.annotationSpan((graph.spans(node.spans(0)).start, graph.spans(node.spans(0)).end))
        return Range(span._1, span._2)
    }

    def ffDependencyPath(node1: Node, node2: Node, label: String) : FeatureVector = {
        val (word1Index, word2Index, path) = (for { w1 <- dependencySpan(node1)
                                                    w2 <- dependencySpan(node2)
                                             } yield { (w1, w2, dependencyPath(w1, w2)) }).minBy(x => x._3._1.size + x._3._2.size)
        // TODO: could also do all paths instead of just the shortest
        val (word1, word2) = (dependencies.tok(word1Index), dependencies.tok(word2Index))
        if (path._1.size + path._2.size <= 4) {
            val pathStr = dependencyPathString(path).mkString("_")
            FeatureVector(Map(("C1="+node1.concept+"+C2="+node2.concept+"+DP="+pathStr+"+L="+label) -> 1.0,
                              ("W1="+word1+"+W2="+word2+"+DP="+pathStr+"+L="+label) -> 1.0,
                              ("W1="+word1+"+DP="+pathStr+"+L="+label) -> 1.0,
                              ("W2="+word2+"+DP="+pathStr+"+L="+label) -> 1.0,
                              ("DP="+pathStr+"+L="+label) -> 1.0
                              ))
        } else {
            FeatureVector()
        }
    }

    // TODO: fDependencyAllPaths

    def dependencyPath(word1: Int, word2: Int) : (List[Int], List[Int]) = {
        // List one is path from word1 to common head
        // List two is path from common head to word2
        // Includes the common head in both lists
        val prefix = rootDependencyPaths(word1).longestCommonPrefixLength(rootDependencyPaths(word2))
        return (rootDependencyPaths(word1).drop(prefix-1).reverse, rootDependencyPaths(word2).drop(prefix-1))
    }

    def dependencyPathString(path: (List[Int], List[Int])) : List[String] = {
        // Assumes that the POS tags use the same tokenization as the dependencies
        var pathList : List[String] = List()
        for (List(word1, word2) <- path._1.sliding(2)) {
            pathList = pos.tok(word1) + "_" + dependencies.annotations.find(x => (x.dependent == word1 && x.head == word2)).get.relation + ">_" + pos.tok(word2) :: pathList
        }
        for (List(word1, word2) <- path._2.sliding(2)) {
            pathList = pos.tok(word1) + "_" + dependencies.annotations.find(x => (x.head == word1 && x.dependent == word2)).get.relation + "<_" + pos.tok(word2) :: pathList
        }
        return pathList.reverse
    }

    def rootDependencyPath(word: Int, path: List[Int] = List()) : List[Int] = {
        // Returns path to root as a list in reverse order (including the word we started at)
        if (word == -1) {
            path
        } else {
            val dep = dependencies.annotations.find(_.dependent == word)
            assert(dep != None, "The dependency tree seems broken.  I can't find the head of "+input.dependencies.tok(word)+" in position "+word)
            rootDependencyPath(dep.get.head, word :: path)
        }
    }

    def ffRootConcept(node: Node) : FeatureVector = {
        logger(2, "ffRootConcept: Node = " + node.concept)
        return FeatureVector(Map(("C="+node.concept+"+L=<ROOT>") -> 1.0))
    }

    // TODO: ffRootDependencyPath

    val rootFeature = List("rootConcept","rootPath")
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

    def localFeatures(node1: Node, node2: Node, label: String) : FeatureVector = {
        // Calculate the local features
        val feats = FeatureVector()
        for (ff <- featureFunctions) {
            feats += ff(node1, node2, label)
        }
        return feats
    }

    def localScore(node1: Node, node2: Node, label: String) : Double = {
        var score = 0.0
        for (ff <- featureFunctions) {
            logger(2, ff.toString)
            logger(2, ff(node1, node2, label))
            score += weights.dot(ff(node1, node2, label))
        }
        return score
    }

    def rootFeatures(node: Node) : FeatureVector = {
        // Calculate the local features
        val feats = FeatureVector()
        for (ff <- rootFeatureFunctions) {
            feats += ff(node)
        }
        return feats
    }

    def rootScore(node: Node) : Double = {
        var score = 0.0
        for (ff <- rootFeatureFunctions) {
            logger(2, ff.toString)
            logger(2, ff(node))
            score += weights.dot(ff(node))
        }
        return score
    }

}

