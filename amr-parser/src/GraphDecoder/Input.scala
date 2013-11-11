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

case class Input(graph: Graph, sentence: Array[String], dependencies: Annotation[Array[Dependency]], pos: Annotation[Array[String]]) {


    def this(amrdata: AMRTriple, dependencies: String, oracle: Boolean, clearUnalignedNodes: Boolean = true) = this(
        if (oracle) {
            amrdata.toOracleGraph(clearUnalignedNodes)
        } else {
            amrdata.toInputGraph
        },
        amrdata.sentence,
        Annotation(amrdata.sentence,
                   dependencies.split("\n").zipWithIndex.map(x => {
                       val StanfordToken = """[^(]+\([^,]+, (.*)-([0-9]+)\)""".r // amod(Academy-5, Riyadh-based-2)
                       val StanfordToken(token,i) = x._1
                       assert(i.toInt-1 == x._2, "The dependencies are in an incorrect order (should be sorted by dependent position).")
                       token }),
                   dependencies.split("\n").map(x => Dependency.fromStanford(x))),
        Array[String]())

}

