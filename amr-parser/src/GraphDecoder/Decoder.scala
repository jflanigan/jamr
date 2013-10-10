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

case class Dependency(head: Int, dependent: Int, relation: String)

case class Input(graph: Graph, sentence: Array[String], parse: Array[Dependency])

case class DecoderResult(graph: Graph, features: FeatureVector, score: Double)

abstract class Decoder(feature_names: List[String], label_set: Array[Label]) {
    val features = new Features(feature_names) // maybe this should be renamed ff?
    val local_score: (Node, Node, Label, Input) => Double = features.local_score
    val local_features: (Node, Node, Label, Input) => FeatureVector = features.local_features

    var labels = label_set
    var nodes = Array.empty[Node]

    var neighbors: (Node) => Iterator[Node] = node => {
        nodes.view.iterator
    }

    def decode(input: Input, labels: Array[Label]) : DecoderResult
}

