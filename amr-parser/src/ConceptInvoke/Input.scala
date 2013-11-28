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

case class Entity(start: Int, end: Int, label: String)

case class Input(sentence: Array[String], notTokenized: Annotation[Nothing], dependencies: Annotation[Array[Dependency]], pos: Annotation[Array[String]], ner: Annotation[Array[Entity]]) {

    def this(sent: Array[String], notTok: Array[String], conllDeps: String, conllNER: String) = this(
        /*if (oracle) {
            amrdata.toOracleGraph(clearUnalignedNodes)
        } else {
            amrdata.toInputGraph
        },*/
        sent,
        Annotation(notTok, sent, null),
        Annotation(amrdata.sentence,
                   conllDeps.split("\n").map(x => x.split("\t")(1)),           // Field 2 is token
                   conllDeps.split("\n").map(x => Dependency.fromConll(x))),
        Annotation(amrdata.sentence,
                   conllDeps.split("\n").map(x => x.split("\t")(1)),           // Field 2 is token
                   conllDeps.split("\n").map(x => x.split("\t")(4)))           // Field 5 is POS
        Annotation(sent,
                   conllNER.split("\n").map(x => x.split("\t")(0)),            // Field 0 is token
                   entitiesFromConll(conllNER)))
    
    def entitiesFromConll(conllStr: String, column: Int = 1) : Array[Entity] = {
        val conll = conllStr.split("\n").map(x => x.split("\t"))    // WARNING: conllStr should not end with a '\n' (otherwise our test for conll(i)(1) != "O" might be index out of bounds because the line is empty)
        var i = 0
        val entities : ArrayBuffer[Entity] = ArrayBuffer()
        while (i < conll.size) {
            if (conll(i)(column) != "O") {
                assert(conll(i)(column).beginsWith(".-"), "NER data is not in conll BIO tagging format")
                 val label = conll(i)(column).drop(2)
                 val start = i
                 i += 1
                 while (conll(i)(column) == "I-"+label && i < conll.size) {
                    i +=1
                 }
                 val end = i
                 entities += Entity(start, end, label)
            } else {
                i += 1
            }
        }
    }

}

