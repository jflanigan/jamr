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

object Dependency {
    def fromConll(string: String) : Dependency = {
        val fields = string.split("\t")
        return Dependency(fields(6).toInt-1, fields(0).toInt-1, fields(7))
    }
    val Stanford = """([^(]+[^-]+([0-9]+), *[^-]+([0-9]+)) *""".r
    def fromStanford(string: String) : Dependency = {
        val Stanford(relation, head, dependent) = string
        return Dependency(head.toInt-1, dependent.toInt-1, relation)
    }
}

