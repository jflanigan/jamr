package edu.cmu.lti.nlp.amr.GraphDecoder
import edu.cmu.lti.nlp.amr._  
import edu.cmu.lti.nlp.amr.FastFeatureVector._

import scala.io.Source.fromFile

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}
import scala.collection.immutable

/**************************** Feature Functions *****************************/

object Features {       // This object is a hack, should change so all inputs are sent in an XML container
    private var firstTime : Boolean = true
    private var srlArray : Array[String] = _
    def setUpInput(options: Map[Symbol,String], featureNames: List[String]) {
        if (firstTime) {
            firstTime = false
            if (featureNames.contains("srl")) {
                srlArray = Corpus.splitOnNewline(fromFile(options('srl)).getLines).toArray
            }
        }
    }
}

class Features(options: Map[Symbol,String], private var myFeatureNames: List[String], labelSet: Array[String]) {
    var weights = FeatureVector(labelSet: Array[String])    // TODO: maybe weights should be passed in to the constructor
    private var inputSave: Input = _
    private var graph: Graph = _
    private var sentence: Array[String] = _
    private var dependencies: Annotation[Dependency] = _
    private var fullPos: Annotation[String] = _
    private var node1 : Node = _
    private var node2 : Node = _
    private var srl: SRL = _
    private var feats : List[(String, Value, immutable.Map[Int, Double])] = _

    def node1Start = graph.spans(node1.spans(0)).start
    def node1End = graph.spans(node1.spans(0)).end
    def node2Start = graph.spans(node2.spans(0)).start
    def node2End = graph.spans(node2.spans(0)).end

    def input: Input = inputSave
    def input_= (i: Input) {
        Features.setUpInput(options, featureNames)
        inputSave = i
        graph = i.graph.get
        sentence = i.sentence
        dependencies = i.dependencies
        fullPos = i.pos
        if  (featureNames.contains("srl")) {
            srl = SRL.fromString(Features.srlArray(i.index), dependencies)
        }
        precompute
    }

    def featureNames: List[String] = myFeatureNames
    def featureNames_= (featNames: List[String]) {
        myFeatureNames= featNames
        featureFunctions = myFeatureNames.filter(x => !rootFFTable.contains(x)).map(x => ffTable(x))     // TODO: error checking on lookup
        rootFeatureFunctions = myFeatureNames.filter(x => rootFFTable.contains(x)).map(x => rootFFTable(x))
    }

    type FeatureFunction = () => Unit           // function with no arguments and no return value

    val ffTable = Map[String, FeatureFunction](
        "CostAugEdge" -> ffCostAugEdge _,   // trailing _ for partially applied function
        "DDEdgeId" -> ffDDEdgeId _,
        "LRLabelWithId" -> ffLRLabelWithId _,
        "bias" -> ffBias _,
        "biasScaled" -> ffBiasScaled _,
        "biasCSuf" -> ffBiasCSuf _,
        "typeBias" -> ffTypeBias _,
        "self" -> ffSelf _,
        "fragHead" -> ffFragHead _,
        "edgeCount" -> ffEdgeCount _,
        "distance" -> ffDistance _,
        "logDistance" -> fflogDistance _,
        "conceptBigram" -> ffConceptBigram _,
        "conceptUnigramWithLabel" -> ffConceptUnigramWithLabel _,
        "posPathv1" -> ffPosPathUnigramBigramv1 _,
        "posPathv2" -> ffPosPathUnigramBigramv2 _,
        "posPathv3" -> ffPosPathUnigramBigramv3 _,
        //"dependencyPathv1" -> ffDependencyPathv1 _,
        "dependencyPathv2" -> ffDependencyPathv2 _,
        "dependencyPathv3" -> ffDependencyPathv3 _,
        "dependencyPathv4" -> ffDependencyPathv4 _,
        "dependencyPathv5" -> ffDependencyPathv5 _,
        "srl" -> ffSRL _
    )

    val rootFFTable = Map[String, FeatureFunction](
        "rootConcept" -> ffRootConcept _,
        "rootCostAug" -> ffRootCostAug _,
        "rootDependencyPathv1" -> ffRootDependencyPathv1 _
    )

    def precompute() {
        rootDependencyPaths = dependencies.tok.indices.map(i => rootDependencyPath(i)).toArray
        logger(1,"rootDependencyPaths = "+rootDependencyPaths.toList)
    }

    // node1 is always the tail, and node2 the head

    def addFeature(feat: String, unconjoined: Double, conjoined: Double) {
        feats = (feat, Value(unconjoined, conjoined), immutable.Map.empty[Int,Double]) :: feats
    }

    def addFeature(feat: String, unconjoined: Double, conjoined: Double, conjoinedMap: immutable.Map[Int, Double]) {
        feats = (feat, Value(unconjoined, conjoined), conjoinedMap) :: feats
    }

    def ffCostAugEdge(n1: Node, n2: Node) : List[(String, Value, immutable.Map[Int, Double])] = {
        node1 = n1
        node2 = n2
        feats = List()
        ffCostAugEdge
        return feats
    }

    def ffCostAugEdge {       // Used for cost augmented decoding
        if (node1.spans(0) != node2.spans(0)) {
            addFeature("CA:U_C1="+node1.concept+"+C2="+node2.concept, 1.0, 0.0) // WARNING: don't change this without also changing the features in CostAugmented decoder as well
            addFeature("CA:C1="+node1.concept+"+C2="+node2.concept, 0.0, 1.0)   // WARNING: don't change this without also changing the features in CostAugmented decoder as well
        }
    }

    def ffDDEdgeId {          // Used for Dual Decomposition
        addFeature("DD:Id1="+node1.id, 0.0, 1.0)
        //return FeatureVector(Map(("DD:Id1="+node1.id+"+Id2="+node2.id+"+L="+label) -> 1.0))
    }

    def ffLRLabelWithId(n1: Node, n2: Node) : List[(String, Value, immutable.Map[Int, Double])] = {
        node1 = n1
        node2 = n2
        feats = List()
        ffLRLabelWithId
        return feats
    }

    def ffLRLabelWithId {     // Used for Langragian Relaxation
        addFeature("LR:Id1="+node1.id, 0.0, 1.0)
        //return FeatureVector(Map(("LR:Id1="+node1.id+"+L="+label) -> 1.0))
    }

    def ffBias {              // TODO: after testing, adjust back to 0.01
        // Bias features are unregularized.  Adjusting these values only adjusts the condition number of the optimization problem. 
        addFeature("Bias", 0.0, 1.0)
        //return FeatureVector(Map(("L="+label) -> 1.0))
    }

    def ffBiasScaled {
        // Bias features are unregularized.  Adjusting these values only adjusts the condition number of the optimization problem. 
        addFeature("Bias.01", 0.01, 0.01)
    }

    // TODO: remove (legacy feature for reproducable results, same as ffBias)
    def ffEdgeCount {
        addFeature("Edge", 1.0, 0.0)
    }

    def ffBiasCSuf {
        val c1size = node1.concept.size
        val c2size = node2.concept.size
        addFeature("C1Suf3="+node1.concept.slice(c1size-3, c1size), 1.0, 1.0)
        addFeature("C2Suf3="+node2.concept.slice(c2size-3, c2size), 1.0, 1.0)
    }

    // TODO
    def ffTypeBias {
        def conceptType(x: String) : String = if (x.matches(".*-[0-9][0-9]")) { "E" } else { "O" }
        def labelType(x: String) : String = if (x.startsWith(":ARG")) { "A" } else { "O" }
        addFeature("C1T="+conceptType(node1.concept), 1.0, 1.0)
        addFeature("C2T="+conceptType(node2.concept), 1.0, 1.0)
        addFeature("C1T="+conceptType(node1.concept)+"LT=A", 0.0, 0.0, weights.labelToIndex.toMap.filter(x => x._1.startsWith(":ARG")).map(x => (x._2, 1.0)))
        addFeature("C2T="+conceptType(node2.concept)+"LT=A", 0.0, 0.0, weights.labelToIndex.toMap.filter(x => x._1.startsWith(":ARG")).map(x => (x._2, 1.0)))
        addFeature("C1T="+conceptType(node1.concept)+"LT=O", 0.0, 0.0, weights.labelToIndex.toMap.filter(x => !x._1.startsWith(":ARG")).map(x => (x._2, 1.0)))
        addFeature("C2T="+conceptType(node2.concept)+"LT=O", 0.0, 0.0, weights.labelToIndex.toMap.filter(x => !x._1.startsWith(":ARG")).map(x => (x._2, 1.0)))
        /*return FeatureVector(Map("C1T="+conceptType(node1.concept)+"LT="+labelType(label) -> 1.0,
                                 "C2T="+conceptType(node2.concept)+"LT="+labelType(label) -> 1.0,
                                 "C1T="+conceptType(node1.concept)+"L="+label -> 1.0,
                                 "C2T="+conceptType(node2.concept)+"L="+label -> 1.0))*/
    }

    def ffSelf {
        addFeature("Slf", if (node1.spans(0) == node2.spans(0)) { 1.0 } else { 0.0 }, 0.0)
        //return FeatureVector(Map("Slf" -> { if (node1.spans(0) == node2.spans(0)) { 1.0 } else { 0.0 } } ))
    }

    def ffFragHead {
        // TODO: I'm assuming it is unlikely there are two identical concepts in a frag
        //logger(1,"fragHead node1.concept = "+node1.concept+" node2.concept = "+node2.concept)
        //logger(1,"fragHead node1.spans = "+node1.spans.toList+" node2.spans = "+node2.spans.toList)
        addFeature("C1NotFragHead", if (node1.concept != graph.spans(node1.spans(0)).amr.concept) { 1.0 } else { 0.0 }, 0.0)
        addFeature("C2NotFragHead", if (node2.concept != graph.spans(node2.spans(0)).amr.concept) { 1.0 } else { 0.0 }, 0.0)
    }

    def ffDistancev0 {
        val distance = min(Math.abs(graph.spans(node1.spans(0)).start - graph.spans(node2.spans(0)).end),
                           Math.abs(graph.spans(node1.spans(0)).end - graph.spans(node2.spans(0)).start))
        val direction = signum(graph.spans(node1.spans(0)).start - graph.spans(node2.spans(0)).end)
        addFeature("abs(dist)", distance, 0.0)
        addFeature("dist", direction*distance, 0.0)
    }

    def ffDistance {
        val distance = min(Math.abs(graph.spans(node1.spans(0)).start - graph.spans(node2.spans(0)).end),
                           Math.abs(graph.spans(node1.spans(0)).end - graph.spans(node2.spans(0)).start))
        val pathStrv3 = depPathStrv3(node1, node2)
        addFeature("d="+min(distance,20).toString, 1.0, 1.0)
        addFeature("d="+min(distance,20).toString+"+"+pathStrv3, 1.0, 1.0)
    }

    def fflogDistance {
        val distance = min(Math.abs(graph.spans(node1.spans(0)).start - graph.spans(node2.spans(0)).end),
                           Math.abs(graph.spans(node1.spans(0)).end - graph.spans(node2.spans(0)).start))
        val pathStrv3 = depPathStrv3(node1, node2)
        addFeature("logD", log(distance+1), log(distance+1))
        addFeature("logD+"+pathStrv3, log(distance+1), log(distance+1))
    }

    def ffDistanceIntervals {
        val distance = min(Math.abs(graph.spans(node1.spans(0)).start - graph.spans(node2.spans(0)).end),
                           Math.abs(graph.spans(node1.spans(0)).end - graph.spans(node2.spans(0)).start))
        val direction = signum(graph.spans(node1.spans(0)).start - graph.spans(node2.spans(0)).end).toInt
        val pathStrv3 = depPathStrv3(node1, node2)
        for (i <- Range(1, distance-1)) {
            addFeature("dint="+(direction*i).toString, 1.0, 1.0)
            addFeature("dint="+(direction*i).toString+"+"+pathStrv3, 1.0, 1.0)
        }
    }

    def fflogDistanceIntervals {
        val distance = min(Math.abs(graph.spans(node1.spans(0)).start - graph.spans(node2.spans(0)).end),
                           Math.abs(graph.spans(node1.spans(0)).end - graph.spans(node2.spans(0)).start))
        val direction = signum(graph.spans(node1.spans(0)).start - graph.spans(node2.spans(0)).end).toInt
        val pathStrv3 = depPathStrv3(node1, node2)
        for (i <- Range(1, Math.floor(log(distance+1)/log(1.39)).toInt - 1)) {
            addFeature("logDi="+(direction*i).toString, 1.0, 1.0)
            addFeature("logDi="+(direction*i).toString+"+"+pathStrv3, 1.0, 1.0)
        }
    }

    def ffDirection {
        val dir = signum(graph.spans(node1.spans(0)).start - graph.spans(node2.spans(0)).end + .01f)
        addFeature("dir", dir, dir)
    }

    def ffConceptBigram {
        //logger(2, "ffConceptBigram: Node1 = " + node1.concept + " Node2 = " + node2.concept + " label = " + label)
        addFeature("C1="+node1.concept+"+C2="+node2.concept, /*1.0*/ 0.0, 1.0)  // TODO: check that adding no label helps
    }

    def ffConceptUnigramWithLabel {
        addFeature("C1="+node1.concept, 0.0, 1.0)
        addFeature("C2="+node2.concept, 0.0, 1.0)
    }

    def ffSRL {
        for ((predType, argLabel) <- srl.relations((node1Start, node1End), (node2Start, node2End))) {
            addFeature("SRL", 1.0, 1.0)
            addFeature("SRL"+predType, 1.0, 1.0)
            addFeature("SRL="+argLabel, 1.0, 1.0)
            addFeature("SRL"+predType+"="+argLabel, 1.0, 1.0)
        }
    }

    def ffPosPathUnigramBigramv1 {
        val posSet1 = posSpanSet(node1).mkString("_")
        val posSet2 = posSpanSet(node2).mkString("_")
        val (unigrams, bigrams) = posPathUnigramAndBigramCounts(node1, node2)
        val pp = "PPv1"
        val direction = if (graph.spans(node1.spans(0)).start < graph.spans(node2.spans(0)).start) { "+1" } else { "-1" }
        val dpStr = depPathStrv3(node1, node2)
        for ((unigram, count) <- unigrams) {
            val ppStr = "C1PS="+posSet1+"+C2PS="+posSet2+"+dir="+direction+"+"+pp+"U="+unigram
            addFeature(ppStr, count, count)
            addFeature(ppStr+"_"+count.toString, 1.0, 1.0)
            addFeature(dpStr+"+"+ppStr, count, count)
            addFeature(dpStr+"+"+ppStr+"_"+count.toString, 1.0, 1.0)
        }
        for ((bigram1, bigram2, count) <- bigrams) {
            val ppStr = "C1PS="+posSet1+"+C2PS="+posSet2+"+dir="+direction+"+"+pp+"B="+bigram1+"_"+bigram2
            addFeature(ppStr, count, count)
            addFeature(ppStr+"_"+count.toString, 1.0, 1.0)
            addFeature(dpStr+"+"+ppStr, count, count)
            addFeature(dpStr+"+"+ppStr+"_"+count.toString, 1.0, 1.0)
        }
    }

    def ffPosPathUnigramBigramv2 {
        val posSet1 = posSpanSet(node1).mkString("_")
        val posSet2 = posSpanSet(node2).mkString("_")
        val (unigrams, bigrams) = posPathUnigramAndBigramCounts(node1, node2)
        val pp = "PPv2"
        val direction = if (graph.spans(node1.spans(0)).start < graph.spans(node2.spans(0)).start) { "+1" } else { "-1" }
        val dpStr = depPathStrv3(node1, node2)
        for ((unigram, count) <- unigrams) {
            val ppStr = "C1PS="+posSet1+"+C2PS="+posSet2+"+dir="+direction+"+"+pp+"U="+unigram
            addFeature(ppStr+"_"+count.toString, 1.0, 1.0)
        }
        for ((bigram1, bigram2, count) <- bigrams) {
            val ppStr = "C1PS="+posSet1+"+C2PS="+posSet2+"+dir="+direction+"+"+pp+"B="+bigram1+"_"+bigram2
            addFeature(ppStr+"_"+count.toString, 1.0, 1.0)
        }
    }

    def ffPosPathUnigramBigramv3 {
        val posSet1 = posSpanSet(node1).mkString("_")
        val posSet2 = posSpanSet(node2).mkString("_")
        val pp = "PPv3"
        val direction = if (graph.spans(node1.spans(0)).start < graph.spans(node2.spans(0)).start) { "+1" } else { "-1" }
        val dpStr = depPathStrv3(node1, node2)

        val span1 = fullPos.annotationSpan((graph.spans(node1.spans(0)).start, graph.spans(node1.spans(0)).end))
        val span2 = fullPos.annotationSpan((graph.spans(node2.spans(0)).start, graph.spans(node2.spans(0)).end))
        val (start, end) = if (span2._1 - span1._2 >= 0) { (span1._2, span2._1) } else { (span2._2, span1._1) }
        val posPath = fullPos.annotation.slice(start, end)
        val ppStr1 = "C1PS="+posSet1+"+C2PS="+posSet2+"+dir="+direction+"+"+pp+".1="+posPath.mkString("_")
        val ppStr2 = "C1PS="+posSet1+"+C2PS="+posSet2+"+dir="+direction+"+"+pp+".2="+posPath.distinct.mkString("_")

        addFeature(ppStr1, 1.0, 1.0)
        addFeature(ppStr1+dpStr, 1.0, 1.0)
        addFeature(ppStr2, 1.0, 1.0)
        addFeature(ppStr2+dpStr, 1.0, 1.0)
    }

    def posPathUnigramAndBigramCounts(node1: Node, node2: Node) : (List[(String, Int)], List[(String, String, Int)]) = {
        val span1 = fullPos.annotationSpan((graph.spans(node1.spans(0)).start, graph.spans(node1.spans(0)).end))
        val span2 = fullPos.annotationSpan((graph.spans(node2.spans(0)).start, graph.spans(node2.spans(0)).end))
        val (start, end) = if (span2._1 - span1._2 >= 0) { (span1._2, span2._1) } else { (span2._2, span1._1) }
        val posPath = fullPos.annotation.slice(start, end).toList   // posPath must be a list otherwise bigramCounts won't work (equality test fails when using arrays)
        val posPathBigrams = posPath.sliding(2).toList
        val unigramCounts = posPath.distinct.map(x => (x, posPath.count(_ == x)))
        val bigramCounts = posPathBigrams.distinct.map(x => (x.getOrElse(0,"NONE"), x.getOrElse(1,"NONE"), posPathBigrams.count(_ == x)))
        return (unigramCounts, bigramCounts)
    }

    def posSpan(node: Node) : List[String] = {
        // The pos labels for the node's span
        val span = fullPos.annotationSpan((graph.spans(node.spans(0)).start, graph.spans(node.spans(0)).end))
        return fullPos.annotation.slice(span._1, span._2).toList
    }

    def posSpanSet(node: Node) : List[String] = {
        // The set of pos labels for the node's span (as a sorted list)
        return posSpan(node).sorted.distinct
    }

    var rootDependencyPaths : Array[List[Int]] = _

    def dependencySpan(node: Node) : Range = {
        // Returns the node's span in the dependency parse
        //logger(1, "node.spans = "+node.spans)
        //logger(1, "node.spans(0) = "+node.spans(0).toString)
        //logger(1,"span = "+(graph.spans(node.spans(0)).start, graph.spans(node.spans(0)).end))
        // If an array index out of bounds error occurs in the line below, it could be because the input sentence has a training newline (I know, fragile!)
        val span = dependencies.annotationSpan((graph.spans(node.spans(0)).start, graph.spans(node.spans(0)).end))
        return Range(span._1, span._2)
    }

    def ffDependencyPathv2 {
        val (word1Index, word2Index, path) = (for { w1 <- dependencySpan(node1)
                                                    w2 <- dependencySpan(node2)
                                             } yield { (w1, w2, dependencyPath(w1, w2)) }).minBy(x => x._3._1.size + x._3._2.size)
        // TODO: could also do all paths instead of just the shortest
        val dp = "DPv2="
        val pos = fullPos
        pos.annotation = fullPos.annotation.map(x => x.replaceAll("VB.*","VB").replaceAll("NN.*|PRP|FW","NN").replaceAll("JJ.*","JJ").replaceAll("RB.*","RB"))
        val (word1, word2) = (dependencies.tok(word1Index), dependencies.tok(word2Index))
        if (path._1.size + path._2.size <= 4) {
            val pathStr = dependencyPathString(path, pos).mkString("_")
            addFeature("C1="+node1.concept+"+C2="+node2.concept+"+"+dp+pathStr, 0.0, 1.0)
            addFeature("W1="+word1+"+W2="+word2+"+"+dp+pathStr, 0.0, 1.0)
            addFeature("W1="+word1+"+"+dp+pathStr, 0.0, 1.0)
            addFeature("W2="+word2+"+"+dp+pathStr, 0.0, 1.0)
            addFeature(dp+pathStr, 0.0, 1.0)
        }
    }

    def ffDependencyPathv3 {
        val (word1Index, word2Index, path) = (
            for { w1 <- dependencySpan(node1)
                  w2 <- dependencySpan(node2)
                } yield { (w1, w2, dependencyPath(w1, w2)) }).minBy(x => x._3._1.size + x._3._2.size)

        // TODO: could also do all paths instead of just the shortest
        val dp = "DPv3="
        val pos = fullPos
        pos.annotation = fullPos.annotation.map(x => x.replaceAll("VB.*","VB").replaceAll("NN.*","NN"))
        val (word1, word2) = (dependencies.tok(word1Index), dependencies.tok(word2Index))
        if (path._1.size + path._2.size <= 4) {  // TODO: max path longer
            val pathStr = dependencyPathString(path, pos).mkString("_")
                              //("C1="+node1.concept+"+C2="+node2.concept+"+"+dp+pathStr+"+L="+label) -> 1.0,
                              //("W1="+word1+"+W2="+word2+"+"+dp+pathStr+"+L="+label) -> 1.0,
            addFeature("C1="+node1.concept+"+"+dp+pathStr, 1.0, 0.0) // TODO: add conjoined
            addFeature("C2="+node2.concept+"+"+dp+pathStr, 1.0, 0.0)
            addFeature("W1="+word1+"+"+dp+pathStr, 1.0, 0.0)
            addFeature("W2="+word2+"+"+dp+pathStr, 1.0, 0.0)
            addFeature(dp+pathStr, 1.0, 1.0)
        } else { 
            addFeature(dp+"NONE", 1.0, 0.0)     // TODO: change to 1.0 for conjoined
        }
    }

    def ffDependencyPathv4 {
        val (word1Index, word2Index, path) = (
            for { w1 <- dependencySpan(node1)
                  w2 <- dependencySpan(node2)
                } yield { (w1, w2, dependencyPath(w1, w2)) }).minBy(x => x._3._1.size + x._3._2.size)

        // TODO: could also do all paths instead of just the shortest
        val dp = "DPv4="
        val pos = fullPos
        pos.annotation = fullPos.annotation.map(x => x.replaceAll("VB.*","VB").replaceAll("NN.*","NN"))
        val (word1, word2) = (dependencies.tok(word1Index), dependencies.tok(word2Index))
        if (path._1.size + path._2.size <= 6) {  // TODO: max path longer
            val pathStr = dependencyPathString(path, pos).mkString("_")
                              //("C1="+node1.concept+"+C2="+node2.concept+"+"+dp+pathStr+"+L="+label) -> 1.0,
                              //("W1="+word1+"+W2="+word2+"+"+dp+pathStr+"+L="+label) -> 1.0,
            addFeature(dp+pathStr, 1.0, 1.0)
            addFeature("C1="+node1.concept+"+"+dp+pathStr, 1.0, 1.0)
            addFeature("C2="+node2.concept+"+"+dp+pathStr, 1.0, 1.0)
            addFeature("W1="+word1+"+"+dp+pathStr, 1.0, 1.0)
            addFeature("W2="+word2+"+"+dp+pathStr, 1.0, 1.0)
        } else { 
            addFeature(dp+"NONE", 1.0, 1.0)
        }
    }

    def ffDependencyPathv5 {
        val (word1Index, word2Index, path) = (
            for { w1 <- dependencySpan(node1)
                  w2 <- dependencySpan(node2)
                } yield { (w1, w2, dependencyPath(w1, w2)) }).minBy(x => x._3._1.size + x._3._2.size)

        // TODO: could also do all paths instead of just the shortest
        val dp = "DPv5="
        val pos = fullPos
        pos.annotation = fullPos.annotation.map(x => x.replaceAll("VB.*","VB").replaceAll("NN.*","NN"))
        val (word1, word2) = (dependencies.tok(word1Index), dependencies.tok(word2Index))
        if (path._1.size + path._2.size <= 6) {  // TODO: max path longer
            val pathStr = dependencyPathStringv2(path, pos).mkString("_")
            addFeature(dp+pathStr, 1.0, 1.0)
            addFeature("C1="+node1.concept+"+"+dp+pathStr, 1.0, 1.0)
            addFeature("C2="+node2.concept+"+"+dp+pathStr, 1.0, 1.0)
            addFeature("W1="+word1+"+"+dp+pathStr, 1.0, 1.0)
            addFeature("W2="+word2+"+"+dp+pathStr, 1.0, 1.0)
        } else {
            addFeature(dp+"NONE", 1.0, 1.0)
        }
    }

    def depPathStrv3(node1: Node, node2: Node, maxpath: Int = 4) : String = {   // same code as above, just only computes pathStr
        val (word1Index, word2Index, path) = (for { w1 <- dependencySpan(node1)
                                                    w2 <- dependencySpan(node2)
                                             } yield { (w1, w2, dependencyPath(w1, w2)) }).minBy(x => x._3._1.size + x._3._2.size)
        val pos = fullPos
        pos.annotation = fullPos.annotation.map(x => x.replaceAll("VB.*","VB").replaceAll("NN.*","NN"))
        "DPv3="+(if (path._1.size + path._2.size <= maxpath) {
            dependencyPathString(path, pos).mkString("_")
        } else {
            "NONE"
        })
    }

    // TODO: ffDependencyAllPaths

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

    def dependencyPathString(path: (List[Int], List[Int]), pos: Annotation[String]) : List[String] = {
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

    def dependencyPathStringv2(path: (List[Int], List[Int]), pos: Annotation[String]) : List[String] = {
        // Assumes that the POS tags use the same tokenization as the dependencies
        // Includes prepositions in the path
        //logger(2, "path="+path.toString)
        var pathList : List[String] = List()
        def posAnnotation(word: Int) : String = {
            if (pos.annotations(word) == "IN") {
                val (start, stop) = dependencies.getSpan(word, word+1)
                sentence.slice(start, stop).mkString("_").toLowerCase
            } else {
                pos.annotations(word)
            }
        }
        for (List(word1, word2) <- path._1.sliding(2)) {
            //logger(2, "Looking for dependent="+word1.toString+" head="+word2.toString)
            pathList = posAnnotation(word1) + "_" + dependencies.annotations.find(x => (x.dependent == word1 && x.head == word2)).get.relation + ">_" + posAnnotation(word2) :: pathList
        }
        for (List(word1, word2) <- path._2.sliding(2)) {
            //logger(2, "Looking for dependent="+word2.toString+" head="+word1.toString)
            pathList = posAnnotation(word1) + "_" + dependencies.annotations.find(x => (x.head == word1 && x.dependent == word2)).get.relation + "<_" + posAnnotation(word2) :: pathList
        }
        return pathList.reverse
    }

    def rootDependencyPath(word: Int, path: List[Int] = List()) : List[Int] = {
        // Returns path to root as a list in reverse order (including the word we started at)
        if (word == -1) {
            path
        } else {
            val dep = dependencies.annotations.find(_.dependent == word)
            if (dep == None) {
                logger(0, " *** WARNING: The dependency tree seems broken.  I can't find the head of "+input.dependencies.tok(word)+" in position "+word)
                List()
            } else {
                rootDependencyPath(dep.get.head, word :: path)
            }
        }
    }

    /* *********************** Note ***********************

        All root features should add "+L=<ROOT" to the string
        and do addFeature(featstring, 1.0, 0.0) 
        (i.e. leave the unconjoined features blank)

       ************************************************** */

    def ffRootConcept = {
        addFeature("C="+node1.concept+"+L=<ROOT>", 1.0, 0.0)
    }

    def ffRootCostAug {         // Used for cost augmented decoding
        addFeature("CA:C1="+node1.concept+"+L=<ROOT>", 1.0, 0.0)
    }

    def ffRootDependencyPathv1 = {
        if (graph.spans.size > 0 && node1.spans.size > 0) {
            val path = dependencyPathString((List(), dependencySpan(node1).map(w => rootDependencyPaths(w)).minBy(x => x.size)), fullPos).mkString("_")
            addFeature("DPRv1="+path+"+L=<ROOT>", 1.0, 0.0)
        }
    }

    var featureFunctions : List[FeatureFunction] = {
        featureNames.filter(x => !rootFFTable.contains(x)).map(x =>
            if(ffTable.contains(x)) {
                ffTable(x)
            } else {
                System.err.println("Error: Unknown feature "+x)
                sys.exit(1).asInstanceOf[Nothing]
            })
    }

    var rootFeatureFunctions : List[FeatureFunction] = {
        featureNames.filter(x => rootFFTable.contains(x)).map(x => rootFFTable(x))
    }

    def addFeatureFunction(featureName: String) {
        if (!featureNames.contains(featureName)) {
            featureNames = featureName :: myFeatureNames    // featureNames is a parametric field, and it updates featureFunctions
        }
    }

    def localFeatures(n1: Node, n2: Node) : List[(String, Value, immutable.Map[Int, Double])] = {
        // Calculate the local features
        assert(featureNames.size == featureFunctions.size+rootFeatureFunctions.size, "featureNames.size != featureFunctions.size.  You must make sure to use setFeatures() or addFeatureFunction() to set the features if you change them after creating the decoder object.")
        node1 = n1
        node2 = n2
        feats = List()
        for (ff <- featureFunctions) {
            ff()
        }
        //logger(1, "localFeatures("+n1.id.toString+","+n2.id.toString+") = \n"+feats.sortBy(_._1).mkString("\n"))
        return feats
    }

    def localFeatures(node1: Node, node2: Node, label: Int) : List[(String, ValuesList)] = {
        return localFeatures(node1, node2).map(
            x => (x._1, ValuesList(x._2.unconjoined + x._3.getOrElse(label, 0.0), List(Conjoined(label, x._2.conjoined)))))
    }

    def localFeatures(node1: Node, node2: Node, label: String) : List[(String, ValuesList)] = {
        if(weights.labelToIndex.contains(label)) {
            localFeatures(node1, node2, weights.labelToIndex(label))
        } else {
            logger(0, "************* WARNING: Cannot find label = "+label+" in the labelset ***************")
            localFeatures(node1, node2).map(x => (x._1, ValuesList(x._2.unconjoined, List())))
        }
    }

    def localScore(node1: Node, node2: Node, label: String) : Double = {
        return weights.dot(localFeatures(node1, node2, label))
    }

    def rootFeatures(node: Node) : List[(String, ValuesList)] = {
        // Calculate the local features
        node1 = node
        feats = List()
        for (ff <- rootFeatureFunctions) {
            ff()
        }
        return feats.map(x => (x._1, ValuesList(x._2.unconjoined, List())))
    }

    def rootScore(node: Node) : Double = {
        return weights.dot(rootFeatures(node))
    }
}

