package edu.cmu.lti.nlp.amr

import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable => m, immutable => i}

case class SRLArg(start: Int, end: Int, label: String)
case class SRLPred(predType: String, start: Int, end: Int, args: List[SRLArg])
case class SRL(preds: List[SRLPred], anno: Annotation[Nothing], deps: Annotation[Dependency]) {
    def relations(head: (Int,Int), dep: (Int,Int)) : List[(String, String)] = {
        val headAnno : (Int,Int) = anno.annotationSpan(head)
        val depAnno : (Int,Int) = anno.annotationSpan(dep)
        var result : List[(String, String)] = List()
        for { head_i <- Range(headAnno._1, headAnno._2)
              dep_i <- Range(depAnno._1, depAnno._2)
            } {
                result = relationMap.getOrElse((head_i, dep_i), List()) ::: result
         }
        return result
    }
    private val heads : i.Map[Int, Int] = deps.annotation.map(x => (x.head, x.dependent)).toMap   // map from dependent index to head index // TODO: move this to a dependency class

    private val relationMap : m.Map[(Int, Int), List[(String, String)]] = m.Map()
    // Populate the relationMap
    for { pred <- preds
          arg <- pred.args
          dep <- Range(arg.start, arg.end)
          if heads.contains(dep) && (heads(dep) < arg.start || heads(dep) >= arg.end)   // its a head of the span
          head_i <- Range(pred.start, pred.end)
        } {
            relationMap((head_i, dep)) = (pred.predType, arg.label) :: relationMap.getOrElse((head_i, dep), List())
    }
}

object SRL {
    def fromString(string: String, deps: Annotation[Dependency]) : SRL = {
        // Example:
        // V:4-5 AM-TMP:0-3
        // V:16-17 AM-LOC:17-18 A1:6-14 AM-PNC:18-25
        // V:19-20 A0:6-14 AM-LOC:22-25 A1:20-22
        val lines = string.splitStr("\n")
        val snt = deps.snt
        val tok = lines(0).splitStr(" ").toArray    // First line is tokenized string
        val preds : List[SRLPred] = lines.tail.map(x => SRLPredFromString(x)).toList
        return SRL(preds, Annotation[Nothing](snt, tok, new Array(0)), deps)
    }
    private val extractor = """(.*):([0-9]+)-([0-9]+)""".r
    private def SRLPredFromString(string: String) : SRLPred = {
        val fields = string.splitStr(" ")
        val extractor(predType, predStart, predEnd) = fields(0)     // First field is predType and pred span
        val args : List[SRLArg] = fields.tail.map(field => {
            val extractor(label, start, end) = field
            SRLArg(start.toInt, end.toInt, label)
        }).toList
        return SRLPred(predType, predStart.toInt, predEnd.toInt, args)
    }
}

