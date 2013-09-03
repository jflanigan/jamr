package edu.cmu.lti.nlp.ner

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

/**************************** Feature Vectors *******************************/

case class mul(scale: Double, v: FeatureVector);
// Trickyness below: see p.452 Programming Scala 2nd Edition (21.5 Implicit conversions)
case class MulAssoc(x: Double) { def * (v: FeatureVector) = mul(x, v) }

case class FeatureVector(fmap : Map[String, Double] = Map[String, Double]()) {
//    def copy(v: FeatureVector) = { FeatureVector(v.fmap.clone()) }
    def dot(v: FeatureVector) : Double = {
        if (fmap.size <= v.fmap.size) {
            (fmap :\ 0.0)((f, sum) => f._2 * v.fmap(f._1) + sum)
        } else {
            (v.fmap :\ 0.0)((f, sum) => fmap(f._1) * f._2 + sum)
        }
    }
    def += (v: FeatureVector) : Unit = {
        for ((feat, value) <- v.fmap) {
            fmap(feat) = fmap.getOrElse(feat,0.0) + value
        }
    }
    def += (m: mul) : Unit = {
        val mul(scale, v) = m
        for ((feat, value) <- v.fmap) {
            fmap(feat) = fmap.getOrElse(feat,0.0) + scale * value
        }
    }
    def -= (v: FeatureVector) : Unit = this += -1.0 * v
    def * (scale: Double) = mul(scale, this)
    def read(filename: String) = {
        val regex = """([^ \t]*)[ \t]*([^ \t]*)""".r
        val iterator = Source.fromFile(filename).getLines()
        fmap.clear()
        fmap ++= iterator.map((s : String) => { val regex(f,v) = s; (f,v.toDouble) })
    }
}

