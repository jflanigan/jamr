package edu.cmu.lti.nlp.amr.ConceptInvoke
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

case class Input(sentence: Array[String], notTokenized: Annotation[Nothing], dependencies: Annotation[Array[Dependency]], pos: Annotation[Array[String]], ner: Annotation[Array[String]]) {

    def this(amrdata: AMRData, conllx: String, oracle: Boolean, clearUnalignedNodes: Boolean = true) = this(
        if (oracle) {
            amrdata.toOracleGraph(clearUnalignedNodes)
        } else {
            amrdata.toInputGraph
        },
        amrdata.sentence,
        Annotation(amrdata.sentence,
                   conllx.split("\n").map(x => x.split("\t")(1)),           // Field 2 is token
                   conllx.split("\n").map(x => Dependency.fromConll(x))),
        Annotation(amrdata.sentence,
                   conllx.split("\n").map(x => x.split("\t")(1)),           // Field 2 is token
                   conllx.split("\n").map(x => x.split("\t")(4))))          // Field 5 is POS
                      //x.split("\t")(4).replaceAll("VB.*","VB").replaceAll("NN.*|PRP|FW","NN").replaceAll("JJ.*","JJ").replaceAll("RB.*","RB"))))

}

