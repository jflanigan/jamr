package edu.cmu.lti.nlp.amr.BasicFeatureVector
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.Train.AbstractFeatureVector

import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable => m, immutable => i}  // m.Set, m.Map, i.Set, i.Map


/**************************** Feature Vectors *******************************/

case class mul(scale: Double, v: FeatureVector);
// Trickyness below: see p.452 Programming Scala 2nd Edition (21.5 Implicit conversions)
case class MulAssoc(x: Double) { def * (v: FeatureVector) = mul(x, v) }
// in package.scala:
// implicit def doubleToMulAssoc(x: Double) = new MulAssoc(x)

case class FeatureVector(fmap : m.Map[String, Double] = m.Map[String, Double]()) extends AbstractFeatureVector(Array()) {
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
    def += (v: AbstractFeatureVector) = { this.+=(v.asInstanceOf[FeatureVector]) }
    def -= (v: AbstractFeatureVector) = { this.-=(v.asInstanceOf[FeatureVector]) }
    def -= (v: FeatureVector) : Unit = this += -1.0 * v
    def -= (m: mul) : Unit = this += mul(-m.scale, m.v)
    def * (scale: Double) = mul(scale, this)
    def nonzero : Boolean = {
        var result = false
        for ((feat, value) <- fmap) {
            result = result || (value != 0.0)
        }
        return result
    }
    def slice(v: FeatureVector) : FeatureVector = {
        val f = FeatureVector()
        for ((feat, _) <- v.fmap) {
            f.fmap(feat) = fmap.getOrElse(feat,0.0)
        }
        return f
    }
    def slice(func: String => Boolean) : FeatureVector = {
        val f = FeatureVector()
        for ((feat, value) <- fmap if func(feat)) {
            f.fmap(feat) = value
        }
        return f
    }
    def read(iterator: Iterator[String]) {    // TODO: make this another constructor
        val regex = """(.*)[ \t]([^ \t]*)""".r
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
    def toCdecFormat() : String = { 
        // converts the feature vector to a one-line string format compatible with cdec's format
        return fmap.toList.map(x => { assert(!x._1.contains(' '), "Cannot convert to cdec format (there should be no spaces in the feature names)");  x._1+"="+x._2 }).sorted.mkString(" ")
    }
    def fromCdecFormat(string: String) {
        val regex = """(.*)=([^=]*)""".r
        fmap.clear()
        fmap ++= string.splitStr(" ").map((s : String) => { val regex(f,v) = s; (f,v.toDouble) })
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

