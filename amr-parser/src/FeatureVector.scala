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
            (fmap :\ 0.0)((f, sum) => f._2 * v.fmap.getOrElse(f._1, 0.0) + sum)
        } else {
            (v.fmap :\ 0.0)((f, sum) => fmap.getOrElse(f._1, 0.0) * f._2 + sum)
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
    def slice(v: FeatureVector) : FeatureVector = {
        val f = FeatureVector()
        for ((feat, _) <- v.fmap) {
            f.fmap(feat) = fmap.getOrElse(feat,0.0)
        }
        return f
    }
    def read(iterator: Iterator[String]) = {    // TODO: make this another constructor
        val regex = """([^ \t]*)[ \t]*([^ \t]*)""".r
        //val iterator = Source.fromFile(filename).getLines()
        fmap.clear()
        fmap ++= iterator.map((s : String) => { val regex(f,v) = s; (f,v.toDouble) })
    }
    override def toString() : String = {
        val string = new StringBuilder
        for (key <- fmap.keys.toList.sorted) {
            if (fmap(key) != 0.0) {
                string.append(key + " " + fmap(key).toString + "\n")
            }
        }
        return string.toString
    }
    def unsorted() : String = {
        val string = new StringBuilder
        for ((key, value) <- fmap) {
            if (fmap(key) != 0.0) {
                string.append(key + " " + value.toString + "\n")
            }
        }
        return string.toString
    }
    def apply(feature_name: String) : Double = {
        fmap.getOrElse(feature_name, 0.0)
    }
}

