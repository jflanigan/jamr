package edu.cmu.lti.nlp.amr.GraphDecoder
import edu.cmu.lti.nlp.amr._

import java.io.File
import java.io.FileOutputStream
import java.io.PrintStream
import java.io.BufferedOutputStream
import java.io.OutputStreamWriter
import java.lang.Math
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
    private var fullPos: Annotation[Array[String]] = _
    //private var pos: Annotation[Array[String]] = _

    def input: Input = Input(graph, sentence, dependencies, fullPos)
    def input_= (i: Input) {
        graph = i.graph
        sentence = i.sentence
        dependencies = i.dependencies
        //pos = i.pos
        fullPos = i.pos
        precompute
    }

    type FeatureFunction = (Node, Node, String) => FeatureVector
    type RootFeatureFunction = (Node) => FeatureVector

    val ffTable = Map[String, FeatureFunction](
        "edgeId" -> ffEdgeId,
        "bias" -> ffBias,
        "biasCSuf" -> ffBiasCSuf,
        "self" -> ffSelf,
        "fragHead" -> ffFragHead,
        "edgeCount" -> ffEdgeCount,
        "distance" -> ffDistance,
        "logDistance" -> fflogDistance,
        "conceptBigram" -> ffConceptBigram,
        "conceptUnigramWithLabel" -> ffConceptUnigramWithLabel,
        //"dependencyPathv1" -> ffDependencyPathv1,
        "dependencyPathv2" -> ffDependencyPathv2,
        "dependencyPathv3" -> ffDependencyPathv3
    )

    val rootFFTable = Map[String, RootFeatureFunction](
        "rootConcept" -> ffRootConcept
    )

    def precompute() {
        rootDependencyPaths = dependencies.tok.indices.map(i => rootDependencyPath(i)).toArray
        logger(1,"rootDependencyPaths = "+rootDependencyPaths.toList)
    }

    // node1 is always the tail, and node2 the head

    def ffEdgeId(node1: Node, node2: Node, label: String) : FeatureVector = {  
        return FeatureVector(Map(("Id1="+node1.id+"+Id2="+node2.id+"+L="+label) -> 1.0))
    }

    def ffBias(node1: Node, node2: Node, label: String) : FeatureVector = {
        return FeatureVector(Map(("L="+label) -> 1.0))
    }

    def ffBiasCSuf(node1: Node, node2: Node, label: String) : FeatureVector = {
        val c1size = node1.concept.size
        val c2size = node2.concept.size
        return FeatureVector(Map(("C1Suf3="+node1.concept.slice(c1size-3, c1size)) -> 1.0,
                                 ("C1Suf3="+node1.concept.slice(c1size-3, c1size)+"L="+label) -> 1.0,
                                 ("C2Suf3="+node2.concept.slice(c2size-3, c2size)) -> 1.0,
                                 ("C2Suf3="+node2.concept.slice(c2size-3, c2size)+"L="+label) -> 1.0))
    }

    def ffSelf(node1: Node, node2: Node, label: String) : FeatureVector = {
        return FeatureVector(Map("Slf" -> { if (node1.spans(0) == node2.spans(0)) { 1.0 } else { 0.0 } } ))
    }

    def ffFragHead(node1: Node, node2: Node, label: String) : FeatureVector = {
        // TODO: I'm assuming it is unlikely there are two identical concepts in a frag
        return FeatureVector(Map("C1NotFragHead" -> { if (node1.concept != graph.spans(node1.spans(0)).amr.concept) { 1.0 } else { 0.0 } }, 
                                 "C2NotFragHead" -> { if (node2.concept != graph.spans(node2.spans(0)).amr.concept) { 1.0 } else { 0.0 } }))
    }

    def ffEdgeCount(node1: Node, node2: Node, label: String) : FeatureVector = {
        return FeatureVector(Map(("Edge") -> 1.0))
    }

    def ffDistance(node1: Node, node2: Node, label: String) : FeatureVector = {
        val distance = min(Math.abs(graph.spans(node1.spans(0)).start - graph.spans(node2.spans(0)).end),
                           Math.abs(graph.spans(node1.spans(0)).end - graph.spans(node2.spans(0)).start))
        return FeatureVector(Map("d="+min(distance,20).toString -> 1.0,
                                 "d="+min(distance,20).toString+"+L="+label -> 1.0))
    }

    def fflogDistance(node1: Node, node2: Node, label: String) : FeatureVector = {
        val distance = min(Math.abs(graph.spans(node1.spans(0)).start - graph.spans(node2.spans(0)).end),
                           Math.abs(graph.spans(node1.spans(0)).end - graph.spans(node2.spans(0)).start))
        return FeatureVector(Map("logD" -> log(distance+1),
                                 "logD+L="+label -> log(distance+1)))
    }

    def ffConceptBigram(node1: Node, node2: Node, label: String) : FeatureVector = {
        //logger(2, "ffConceptBigram: Node1 = " + node1.concept + " Node2 = " + node2.concept + " label = " + label)
        return FeatureVector(Map(/*("C1="+node1.concept+":C2="+node2.concept) -> 1.0,*/
                                 ("C1="+node1.concept+"+C2="+node2.concept+"+L="+label) -> 1.0))
    }

    def ffConceptUnigramWithLabel(node1: Node, node2: Node, label: String) : FeatureVector = {
        return FeatureVector(Map(("C1="+node1.concept+"+L="+label) -> 1.0,
                                 ("C2="+node2.concept+"+L="+label) -> 1.0))
    }

    var rootDependencyPaths : Array[List[Int]] = _

    def dependencySpan(node: Node) : Range = {
        // Returns the node's span in the dependency parse
        //logger(1, "node.spans = "+node.spans)
        //logger(1, "node.spans(0) = "+node.spans(0).toString)
        //logger(1,"span = "+(graph.spans(node.spans(0)).start, graph.spans(node.spans(0)).end))
        val span = dependencies.annotationSpan((graph.spans(node.spans(0)).start, graph.spans(node.spans(0)).end))
        return Range(span._1, span._2)
    }

    def ffDependencyPathv2(node1: Node, node2: Node, label: String) : FeatureVector = {
        val (word1Index, word2Index, path) = (for { w1 <- dependencySpan(node1)
                                                    w2 <- dependencySpan(node2)
                                             } yield { (w1, w2, dependencyPath(w1, w2)) }).minBy(x => x._3._1.size + x._3._2.size)
        // TODO: could also do all paths instead of just the shortest
        val dp = "DPv2="
        val pos = fullPos
        pos.annotation = fullPos.annotation.map(x => x.replaceAll("VB.*","VB").replaceAll("NN.*|PRP|FW","NN").replaceAll("JJ.*","JJ").replaceAll("RB.*","RB"))
        val (word1, word2) = (dependencies.tok(word1Index), dependencies.tok(word2Index))
        val feats = if (path._1.size + path._2.size <= 4) {
            val pathStr = dependencyPathString(path, pos).mkString("_")
            FeatureVector(Map(("C1="+node1.concept+"+C2="+node2.concept+"+"+dp+pathStr+"+L="+label) -> 1.0,
                              ("W1="+word1+"+W2="+word2+"+"+dp+pathStr+"+L="+label) -> 1.0,
                              ("W1="+word1+"+"+dp+pathStr+"+L="+label) -> 1.0,
                              ("W2="+word2+"+"+dp+pathStr+"+L="+label) -> 1.0,
                              (dp+pathStr+"+L="+label) -> 1.0
                              ))
        } else { FeatureVector() }
        return feats
    }

    def ffDependencyPathv3(node1: Node, node2: Node, label: String) : FeatureVector = {
        val (word1Index, word2Index, path) = (for { w1 <- dependencySpan(node1)
                                                    w2 <- dependencySpan(node2)
                                             } yield { (w1, w2, dependencyPath(w1, w2)) }).minBy(x => x._3._1.size + x._3._2.size)
        // TODO: could also do all paths instead of just the shortest
        val dp = "DPv3="
        val pos = fullPos
        pos.annotation = fullPos.annotation.map(x => x.replaceAll("VB.*","VB").replaceAll("NN.*","NN"))
        val (word1, word2) = (dependencies.tok(word1Index), dependencies.tok(word2Index))
        val feats= if (path._1.size + path._2.size <= 4) {
            val pathStr = dependencyPathString(path, pos).mkString("_")
            FeatureVector(Map(//("C1="+node1.concept+"+C2="+node2.concept+"+"+dp+pathStr+"+L="+label) -> 1.0,
                              //("W1="+word1+"+W2="+word2+"+"+dp+pathStr+"+L="+label) -> 1.0,
                              ("C1="+node1.concept+"+"+dp+pathStr) -> 1.0,
                              ("C2="+node2.concept+"+"+dp+pathStr) -> 1.0,
                              ("W1="+word1+"+"+dp+pathStr) -> 1.0,
                              ("W2="+word2+"+"+dp+pathStr) -> 1.0,
                              (dp+pathStr+"+L="+label) -> 1.0,
                              (dp+pathStr) -> 1.0
                              ))
        } else { FeatureVector(Map(dp+"NONE" -> 1.0)) }
        return feats
    }

    // TODO: fDependencyAllPaths

    def dependencyPath(word1: Int, word2: Int) : (List[Int], List[Int]) = {
        // List one is path from word1 to common head
        // List two is path from common head to word2
        // Includes the common head in both lists
        val prefix = rootDependencyPaths(word1).longestCommonPrefixLength(rootDependencyPaths(word2))
        /*logger(2, "word1 = "+word1.toString)
        logger(2, "word2 = "+word2.toString)
        logger(2, "prefix = "+prefix.toString)
        logger(2, "path = "+(rootDependencyPaths(word1).drop(prefix-1).reverse, rootDependencyPaths(word2).drop(prefix-1)).toString)*/
        return (rootDependencyPaths(word1).drop(prefix-1).reverse, rootDependencyPaths(word2).drop(prefix-1))
    }

    def dependencyPathString(path: (List[Int], List[Int]), pos: Annotation[Array[String]]) : List[String] = {
        // Assumes that the POS tags use the same tokenization as the dependencies
        //logger(2, "path="+path.toString)
        var pathList : List[String] = List()
        for (List(word1, word2) <- path._1.sliding(2)) {
            //logger(2, "Looking for dependent="+word1.toString+" head="+word2.toString)
            pathList = pos.annotations(word1) + "_" + dependencies.annotations.find(x => (x.dependent == word1 && x.head == word2)).get.relation + ">_" + pos.annotations(word2) :: pathList
        }
        for (List(word1, word2) <- path._2.sliding(2)) {
            //logger(2, "Looking for dependent="+word2.toString+" head="+word1.toString)
            pathList = pos.annotations(word1) + "_" + dependencies.annotations.find(x => (x.head == word1 && x.dependent == word2)).get.relation + "<_" + pos.annotations(word2) :: pathList
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
            //logger(2, ff.toString)
            //logger(2, ff(node1, node2, label))
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
            //logger(1, ff.toString)
            //logger(1, ff(node))
            score += weights.dot(ff(node))
        }
        return score
    }

}

