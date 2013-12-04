package edu.cmu.lti.nlp.amr

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

case class Input(graph: Option[Graph],
                 sentence: Array[String],
                 notTokenized: Annotation[Array[String]],
                 dependencies: Annotation[Array[Dependency]],
                 pos: Annotation[Array[String]],
                 ner: Annotation[Array[Entity]]) {

    // This constructor is used for stage1 training and decoding
    def this(graph: Option[Graph], sent: Array[String], notTok: Array[String], conllDeps: String, conllNER: String) = this(
        graph,
        sent,
        Annotation(notTok, sent, notTok),
        Annotation(sent,
                   conllDeps.split("\n").map(x => x.split("\t")(1)),           // Field 2 is token
                   conllDeps.split("\n").map(x => Dependency.fromConll(x))),
        Annotation(sent,
                   conllDeps.split("\n").map(x => x.split("\t")(1)),           // Field 2 is token
                   conllDeps.split("\n").map(x => x.split("\t")(4))),          // Field 5 is POS
        Annotation(sent,
                   conllNER.split("\n").map(x => x.split("\t")(0)),            // Field 0 is token
                   Input.entitiesFromConll(conllNER)))

    // This constructor is used for stage2 decoding
    def this(graph: Graph, sentence: Array[String], conllx: String) = this(
        graph,
        sentence,
        Annotation(sentence, sentence, Array()),
        Annotation(sentence,
                   conllx.split("\n").map(x => x.split("\t")(1)),           // Field 2 is token
                   conllx.split("\n").map(x => Dependency.fromConll(x))),
        Annotation(sentence,
                   conllx.split("\n").map(x => x.split("\t")(1)),           // Field 2 is token
                   conllx.split("\n").map(x => x.split("\t")(4))))          // Field 5 is POS

    // This constructor is used for stage2 training
    def this(amrdata: AMRTrainingData, conllx: String, oracle: Boolean, clearUnalignedNodes: Boolean = true) = this(
        if (oracle) {
            amrdata.toOracleGraph(clearUnalignedNodes)
        } else {
            amrdata.toInputGraph
        },
        amrdata.sentence,
        Annotation(sentence, sentence, Array()),
        Annotation(amrdata.sentence,
                   conllx.split("\n").map(x => x.split("\t")(1)),           // Field 2 is token
                   conllx.split("\n").map(x => Dependency.fromConll(x))),
        Annotation(amrdata.sentence,
                   conllx.split("\n").map(x => x.split("\t")(1)),           // Field 2 is token
                   conllx.split("\n").map(x => x.split("\t")(4))))          // Field 5 is POS
                      //x.split("\t")(4).replaceAll("VB.*","VB").replaceAll("NN.*|PRP|FW","NN").replaceAll("JJ.*","JJ").replaceAll("RB.*","RB"))))

}

