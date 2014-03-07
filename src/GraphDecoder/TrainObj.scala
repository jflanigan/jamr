package edu.cmu.lti.nlp.amr.GraphDecoder
import edu.cmu.lti.nlp.amr._

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
import scala.util.parsing.combinator._
import edu.cmu.lti.nlp.amr.Input.Input

class TrainObj(val options : Map[Symbol, String]) extends edu.cmu.lti.nlp.amr.TrainObj(options) {

    val decoder = Decoder(options)
    val oracle = new Oracle(getFeatures(options))
    val costAug = new CostAugmented(Decoder(options), options.getOrElse('trainingCostScale,"10.0").toDouble)
    val weights = decoder.features.weights
    oracle.features.weights = weights
    costAug.features.weights = weights

    val outputFormat = options.getOrElse('outputFormat,"triples").split(",").toList

    def decode(i: Int) : FeatureVector = {
        val amrdata1 = AMRTrainingData(training(i))
        logger(0, "Sentence:\n"+amrdata1.sentence.mkString(" ")+"\n")
        val result = decoder.decode(Input(amrdata1, input(i), oracle = false))
        logger(0, "Spans:")
        for ((span, i) <- amrdata1.graph.spans.zipWithIndex) {
            logger(0, "Span "+(i+1).toString+":  "+span.words+" => "+span.amr)
        }
        logger(0, "AMR:")
        if (outputFormat.contains("AMR")) {
            logger(0, result.graph.root.prettyString(detail = 1, pretty = true)+"\n")
        }
        if (outputFormat.contains("triples")) {
            //logger(0, result.graph.printTriples(detail = 1)+"\n")
            logger(0, result.graph.printTriples(
                detail = 1,
                extra = (node1, node2, relation) => {
                    "\t"+decoder.features.ffDependencyPathv2(node1, node2, relation).toString.split("\n").filter(_.matches("^C1.*")).toList.toString+"\t"+decoder.features.localScore(node1, node2, relation).toString
                })+"\n")
        }
        logger(1, "Decoder features:\n"+result.features+"\n")
        return result.features
    }

    def oracle(i: Int) : FeatureVector = {
        val amrdata = AMRTrainingData(training(i))
        val result = oracle.decode(Input(amrdata, input(i), oracle = true))
        logger(0, "Oracle:")
        if (outputFormat.contains("AMR")) {
           val result2 = oracle.decode(Input(amrdata, input(i), oracle = true, clearUnalignedNodes = false))
           logger(0, result2.graph.root.prettyString(detail = 1, pretty = true)+"\n")
        }
        if (outputFormat.contains("triples")) {
           //logger(0, result.graph.printTriples(detail = 1)+"\n")
           logger(0, result.graph.printTriples(
                detail = 1,
                extra = (node1, node2, relation) => {
                    "\t"+oracle.features.ffDependencyPathv2(node1, node2, relation).toString.split("\n").filter(_.matches("^C1.*")).toList.toString+"\t"+decoder.features.localScore(node1, node2, relation).toString
                })+"\n")
        }
        //logger(0, "Dependencies:\n"+dependencies(i)+"\n")
        logger(1, "Oracle features:\n"+result.features+"\n")
        return result.features
    }

    def costAugmented(i: Int) : FeatureVector = {
        val amrdata1 = AMRTrainingData(training(i))
        logger(0, "Sentence:\n"+amrdata1.sentence.mkString(" ")+"\n")
        val result = costAug.decode(Input(amrdata1, input(i), oracle = false))
        logger(0, "Spans:")
        for ((span, i) <- amrdata1.graph.spans.zipWithIndex) {
            logger(0, "Span "+(i+1).toString+":  "+span.words+" => "+span.amr)
        }
        logger(0, "AMR:")
        if (outputFormat.contains("AMR")) {
            logger(0, result.graph.root.prettyString(detail = 1, pretty = true)+"\n")
        }
        if (outputFormat.contains("triples")) {
            //logger(0, result.graph.printTriples(detail = 1)+"\n")
            logger(0, result.graph.printTriples(
                detail = 1,
                extra = (node1, node2, relation) => {
                    "\t"+costAug.features.ffDependencyPathv2(node1, node2, relation).toString.split("\n").filter(_.matches("^C1.*")).toList.toString+"\t"+costAug.features.localScore(node1, node2, relation).toString
                })+"\n")
        }
        logger(1, "Decoder features:\n"+result.features+"\n")
        return result.features
    }
}

