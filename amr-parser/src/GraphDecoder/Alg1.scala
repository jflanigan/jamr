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

abstract class Alg1(featureNames: List[String]) extends Decoder(featureNames) {
    // Base class has defined:
    // val features: Features
    // var weight: (Node, Node, String, Input) => Double

    def decode(input: Input, labels: Array[Label]) : DecoderResult = {
        return DecoderResult(Graph.parse("(none)"), new FeatureVector(), 0)
        //var nodes: Array[Node]
    }
}

