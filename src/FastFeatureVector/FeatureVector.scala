package edu.cmu.lti.nlp.amr.FastFeatureVector
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.Train.AbstractFeatureVector

import scala.math.sqrt
//import scala.collection.mutable.Map
import scala.collection.concurrent.{TrieMap => Map}
import scala.collection.immutable
import scala.io.Source

case class fastmul(scale: Double, v: List[(String, ValuesList)])
case class fastmul2(scale: Double, v: FeatureVector)
// Trickyness below: see p.452 Programming Scala 2nd Edition (21.5 Implicit conversions)
case class FastMulAssoc(x: Double) { def * (v: List[(String, ValuesList)]) = fastmul(x, v) }
case class FastMul2Assoc(x: Double) { def * (v: FeatureVector) = fastmul2(x, v) }
//    def * (v: FeatureVector) = fastmul(x, v.fmap.view.toList.map(x => (x._1, ValuesList(x._2.unconjoined, x._2.conjoined.view.toList.map(y => Conjoined(y._1, y._2)).toList))).toList) // TODO: change this to be faster

// In package.scala:
//implicit def doubleToFastMulAssoc(x: Double) = new FastMulAssoc(x)
//implicit def doubleToFastMul2Assoc(x: Double) = new FastMul2Assoc(x)

case class Conjoined(labelIndex: Int, value: Double)

case class ValuesList(var unconjoined: Double, var conjoined: List[Conjoined])
case class ValuesMap(var unconjoined: Double, var conjoined: Map[Int, Double]) {
    override def clone : ValuesMap = { ValuesMap(unconjoined, conjoined.clone) }
}
object ValuesMap {
    def apply() : ValuesMap = {
        return ValuesMap(0.0, Map())
    }
}
case class Value(unconjoined: Double, conjoined: Double)

case class FeatureVector(labelset : Array[String],
                         fmap : Map[String, ValuesMap] = Map()) extends AbstractFeatureVector(labelset) {
    val labelToIndex : Map[String, Int] = Map()
    labelToIndex ++= labelset.zipWithIndex
    def iterateOverLabels(v: List[(String, Value)], f: (Conjoined) => Unit) {
        var unconjoinedTotal : Double = 0.0
        val conjoinedTotal : Array[Double] = labelset.map(x => 0.0)
        for ((feature, value) <- v if fmap.contains(feature)) {
            val myValues : ValuesMap = fmap(feature)
            unconjoinedTotal += myValues.unconjoined * value.unconjoined
            for (myValue <- myValues.conjoined) {
                conjoinedTotal(myValue._1) += myValue._2 * value.conjoined
            }
        }
        for (i <- 0 until labelset.size) {
            f(Conjoined(i, unconjoinedTotal + conjoinedTotal(i)))
        }
    }
    def iterateOverLabels2(v: List[(String, Value, immutable.Map[Int, Double])], f: (Conjoined) => Unit) {
        var unconjoinedTotal : Double = 0.0
        val conjoinedTotal : Array[Double] = labelset.map(x => 0.0)
        for ((feature, value, conjoinedMap) <- v if fmap.contains(feature)) {
            val myValues : ValuesMap = fmap(feature)
            unconjoinedTotal += myValues.unconjoined * value.unconjoined
            for ((labelIndex, value) <- conjoinedMap) {   // reusing value name, but it's ok
                conjoinedTotal(labelIndex) += myValues.unconjoined * value
            }
            for (myValue <- myValues.conjoined) {
                conjoinedTotal(myValue._1) += myValue._2 * value.conjoined
            }
        }
        for (i <- 0 until labelset.size) {
            f(Conjoined(i, unconjoinedTotal + conjoinedTotal(i)))
        }
    }
    def apply(feature: String, label: Option[Int]) : Double = {
        if (fmap.contains(feature)) {
            if (label == None) {
                fmap(feature).unconjoined
            } else {
                fmap(feature).conjoined.getOrElse(label.get, 0.0)
            }
        } else {
            0.0
        }
    }
    def updateList(v: List[(String, ValuesList)], f: (String, Option[Int], Double, Double) => Double) {
        for ((feature, value) <- v) {
            val myValues : ValuesMap = fmap.getOrElseUpdate(feature, ValuesMap(0.0, Map()))
            myValues.unconjoined = f(feature, None, myValues.unconjoined, value.unconjoined)
            for (conjoined <- value.conjoined) {
                myValues.conjoined(conjoined.labelIndex) = f(feature,
                                                             Some(conjoined.labelIndex),
                                                             myValues.conjoined.getOrElse(conjoined.labelIndex, 0.0),
                                                             conjoined.value)
            }
        }
    }
    def += (v: AbstractFeatureVector) = { this.+=(v.asInstanceOf[FeatureVector]) }
    def -= (v: AbstractFeatureVector) = { this.-=(v.asInstanceOf[FeatureVector]) }
    def += (v: List[(String, ValuesList)]) = updateList(v, (feat, label, x, y) => x + y)
    def -= (v: List[(String, ValuesList)]) = updateList(v, (feat, label, x, y) => x - y)
    def updateAll(f: (String, Option[Int], Double) => Double) {
        for ((feature, values) <- fmap) {
            values.unconjoined = f(feature, None, values.unconjoined)
            for (conjoined <- values.conjoined) {
                values.conjoined(conjoined._1) = f(feature,
                                                   Some(conjoined._1),
                                                   conjoined._2)
            }
        }
    }
    def *= (scalar : Double) = updateAll((feat, label, x) => scalar * x)
    def update(v: FeatureVector, f: (String, Option[Int], Double, Double) => Double) {
        for ((feature, values) <- v.fmap) {
            val myValues : ValuesMap = fmap.getOrElseUpdate(feature, ValuesMap(0.0, Map()))
            myValues.unconjoined = f(feature, None, myValues.unconjoined, values.unconjoined)
            for (conjoined <- values.conjoined) {
                myValues.conjoined(conjoined._1) = f(feature,
                                                     Some(conjoined._1),
                                                     myValues.conjoined.getOrElse(conjoined._1, 0.0),
                                                     conjoined._2)
            }
        }
    }
    def += (v: FeatureVector) = update(v, (feat, label, x, y) => x + y)
    def -= (v: FeatureVector) = update(v, (feat, label, x, y) => x - y)
    def updateAll(v: FeatureVector, f: (String, Option[Int], Double, Double) => Double) { // TODO: maybe this should be f: (String, ValuesMap, ValuesMap) => ValuesMap.  And also have + and - for ValuesMap objects
        for ((feature, myValues) <- fmap) {
            val values = v.fmap.getOrElse(feature, ValuesMap())
            myValues.unconjoined = f(feature, None, myValues.unconjoined, values.unconjoined)
            for (conjoined <- myValues.conjoined) {
                myValues.conjoined(conjoined._1) = f(feature,
                                                   Some(conjoined._1),
                                                   conjoined._2,
                                                   values.conjoined.getOrElse(conjoined._1, 0.0))
            }
        }
    }
    def dotDivide(v: FeatureVector) = updateAll(v, (feat, label, x, y) => { if ( y==0.0 ) { x } else {  x / y } } )
    def updateWithFilter(v: FeatureVector, featNames: Iterator[String], f: (String, Option[Int], Double, Double) => Double) {
        for (feature <- featNames) {
            val values = v.fmap.getOrElse(feature, ValuesMap())
            val myValues : ValuesMap = fmap.getOrElseUpdate(feature, ValuesMap())
            myValues.unconjoined = f(feature, None, myValues.unconjoined, values.unconjoined)
            for (conjoined <- values.conjoined) {
                myValues.conjoined(conjoined._1) = f(feature,
                                                     Some(conjoined._1),
                                                     myValues.conjoined.getOrElse(conjoined._1, 0.0),
                                                     conjoined._2)
            }
        }
    }
    def plusEqFilter(v: FeatureVector, featNames: Iterator[String]) = updateWithFilter(v, featNames, (feat, label, x, y) => x + y)
    def minusEqFilter(v: FeatureVector, featNames: Iterator[String]) = updateWithFilter(v, featNames, (feat, label, x, y) => x - y)
    def dot(v: List[(String, ValuesList)]) : Double = {
        var total : Double = 0.0
        for ((feature, value) <- v if fmap.contains(feature)) {
            val myValues : ValuesMap = fmap(feature)
            total += myValues.unconjoined * value.unconjoined
            for (conjoined <- value.conjoined if myValues.conjoined.contains(conjoined.labelIndex)) {
                total += myValues.conjoined(conjoined.labelIndex) * conjoined.value
            }
        }
        return total
    }
    def dot(v: FeatureVector) : Double = {
        //logger(1, "Computing dot product")
        var total : Double = 0.0
        for ((feature, value) <- v.fmap if fmap.contains(feature)) {
            val myValues : ValuesMap = fmap(feature)
            total += myValues.unconjoined * value.unconjoined
            //logger(1, feature + " "+myValues.unconjoined.toString+" * "+value.unconjoined.toString)
            for (conjoined <- value.conjoined if myValues.conjoined.contains(conjoined._1)) {
                total += myValues.conjoined(conjoined._1) * conjoined._2
                //logger(1, feature + "+L="+labelset(conjoined._1)+" "+myValues.conjoined(conjoined._1).toString+" * "+conjoined._2.toString)
            }
        }
        return total
    }
    //def l2norm : Double = sqrt(this.dot(this))
    def += (m: fastmul) = updateList(m.v, (feat, label, x, y) => x + m.scale * y)
    def -= (m: fastmul) = updateList(m.v, (feat, label, x, y) => x - m.scale * y)
    def += (m: fastmul2) = update(m.v, (feat, label, x, y) => x + m.scale * y)
    def -= (m: fastmul2) = update(m.v, (feat, label, x, y) => x - m.scale * y)
    def plusEqFilter(m: fastmul2, featNames: Iterator[String]) = updateWithFilter(m.v, featNames, (feat, label, x, y) => x + m.scale * y)
    def minusEqFilter(m: fastmul2, featNames: Iterator[String]) = updateWithFilter(m.v, featNames, (feat, label, x, y) => x - m.scale * y)
    /*def nonzero : Boolean = {
        var result = false
        for ((feat, value) <- fmap) {
            result = result || (value != 0.0)
        }
        return result
    }
    def slice(v: FeatureVector) : FeatureVector = {
        val f = new FeatureVector()
        for ((feat, _) <- v.fmap) {
            f.fmap(feat) = fmap.getOrElse(feat,0.0)
        }
        return f
    } */
    def read(iterator: Iterator[String]) {
        val regex = ("""(.*?)(\+L=("""+labelset.mkString("|")+"""))?[ \t]([^ \t]*)""").r  // .*? is non-greedy
        // (feature, _, label, value)
        // matches featurename+L=label 1.0
        fmap.clear()
        for (line <- iterator) {
            val regex(feature, _, label, value) = line
            if (!fmap.contains(feature)) {
                fmap(feature) = ValuesMap(0.0, Map())
            }
            if (label == null) {
                fmap(feature).unconjoined = value.toDouble
            } else {
                fmap(feature).conjoined(labelToIndex(label)) = value.toDouble // TODO: catch invalid label errors and print labels
            }
        }
    }
    def fromFile(filename: String) {
        val iterator = Source.fromFile(filename).getLines()
        read(iterator)
    }
    def toFile(filename: String) {
        val file = new java.io.PrintWriter(new java.io.File(filename), "UTF-8")
        try { file.print(this.toString) }
        finally { file.close }
    }
    override def toString() : String = {
        var strings : List[String] = List()
        for ((feature, values) <- fmap) {
            if (values.unconjoined != 0.0) {
                strings = feature + " " + values.unconjoined.toString + "\n" :: strings
            }
            for ((labelIndex, value) <- values.conjoined if value != 0.0) {
                strings = feature + "+L=" + labelset(labelIndex) + " " + value.toString + "\n" :: strings
            }
        }
        return strings.sorted.mkString
    }
    def unsorted() : String = {
        val string = new StringBuilder
        for ((feature, values) <- fmap) {
            if (values.unconjoined != 0.0) {
                string.append(feature + " " + values.unconjoined.toString + "\n")
            }
            for ((labelIndex, value) <- values.conjoined if value != 0.0) {
                string.append(feature + "+L=" + labelset(labelIndex) + " " + value.toString + "\n")
            }
        }
        return string.toString
    }
    def set(input: ((String, Option[Int]), Double)) {
        val ((feature, label), value) = input
        if (!fmap.contains(feature)) {
            fmap(feature) = ValuesMap(0.0, Map())
        }
        if (label == None) {
            fmap(feature).unconjoined = value
        } else {
            fmap(feature).conjoined(label.get) = value
        }
    }
    def filter(f: (String) => Boolean) : FeatureVector = {
        val newvec = FeatureVector(labelset)
        for ((feature, value) <- fmap if f(feature)) {
            newvec.fmap(feature) = value.clone
        }
        return newvec
    }
}

object FeatureVector {
    def apply(labelset: Array[String], v: List[(String, ValuesList)]) : FeatureVector = {
        val newvec = FeatureVector(labelset)
        newvec += v
        return newvec
    }
}

