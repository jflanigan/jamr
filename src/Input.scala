package edu.cmu.lti.nlp.amr

import java.lang.Math.abs
import java.lang.Math.log
import java.lang.Math.exp
import java.lang.Math.random
import java.lang.Math.floor
import java.lang.Math.min
import java.lang.Math.max
import scala.io.Source.fromFile
import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

case class Input(var graph: Option[Graph],  // var so we can update for the input to stage 2. TODO: maybe should remove?
                 sentence: Array[String],
                 notTokenized: Annotation[Array[String]],
                 dependencies: Annotation[Array[Dependency]],
                 pos: Annotation[Array[String]],
                 ner: Annotation[Array[Entity]]) {

    // TODO: switch everything to this constructor (the others are unnessary)
    // This constructor is used for stage1 training and decoding
    def this(graph: Option[Graph], sent: Array[String], notTok: Array[String], conllDeps: String, conllNER: String) = this(
        graph,
        sent,
        Annotation(notTok, sent, notTok),
        Annotation(sent,
                   conllDeps.split("\n").map(x => x.split("\t")(1)),    // Field 2 is token
                   conllDeps.split("\n").map(x => Dependency.fromConll(x))),
        Annotation(sent,
                   conllDeps.split("\n").map(x => x.split("\t")(1)),    // Field 2 is token
                   conllDeps.split("\n").map(x => x.split("\t")(4))),   // Field 5 is POS
        Annotation(sent,
                   conllNER.split("\n").map(x => x.split("\t")(0)),     // Field 0 is token
                   Entity.entitiesFromConll(conllNER))
    )

    /*def this(amrdata: AMRTrainingData, input: Input, oracle: Boolean, clearUnalignedNodes: Boolean = true) = this(
        Some(if (oracle) {
            amrdata.toOracleGraph(clearUnalignedNodes)
        } else {
            amrdata.toInputGraph
        }),
        input.sentence, input.notTokenized, input.dependencies, input.pos, input.ner
    )*/

// TODO: Can delete other constructors I think
    // This constructor is used for stage2 decoding
    def this(graph: Graph, sentence: Array[String], conllx: String) = this(
        Some(graph),
        sentence,
        Annotation(sentence, sentence, Array()),
        Annotation(sentence,
                   conllx.split("\n").map(x => x.split("\t")(1)),       // Field 2 is token
                   conllx.split("\n").map(x => Dependency.fromConll(x))),
        Annotation(sentence,
                   conllx.split("\n").map(x => x.split("\t")(1)),       // Field 2 is token
                   conllx.split("\n").map(x => x.split("\t")(4))),      // Field 5 is POS
        Annotation(sentence, sentence, Array()))

    // This constructor is used for stage2 training
    def this(amrdata: AMRTrainingData, conllx: String, oracle: Boolean, clearUnalignedNodes: Boolean = true) = this(
        Some(if (oracle) {
            amrdata.toOracleGraph(clearUnalignedNodes)
        } else {
            amrdata.toInputGraph
        }),
        amrdata.sentence,
        Annotation(amrdata.sentence, amrdata.sentence, Array()),
        Annotation(amrdata.sentence,
                   conllx.split("\n").map(x => x.split("\t")(1)),       // Field 2 is token
                   conllx.split("\n").map(x => Dependency.fromConll(x))),
        Annotation(amrdata.sentence,
                   conllx.split("\n").map(x => x.split("\t")(1)),       // Field 2 is token
                   conllx.split("\n").map(x => x.split("\t")(4))),      // Field 5 is POS
                      //x.split("\t")(4).replaceAll("VB.*","VB").replaceAll("NN.*|PRP|FW","NN").replaceAll("JJ.*","JJ").replaceAll("RB.*","RB"))))
        Annotation(amrdata.sentence, amrdata.sentence, Array()))
}

object Input {
    def loadInputfiles(options: Map[Symbol, String]) : Array[Input] = {
        logger(1,"Loading external input...")
        val tokenized = fromFile(options('tokenized)).getLines.toArray
        val notTokenized = fromFile(options('notTokenized)).getLines.toArray
        val dependencies = if (options.contains('dependencies)) {
            (for {
                block <- Corpus.splitOnNewline(fromFile(options('dependencies).asInstanceOf[String]).getLines())
            } yield block.replaceAllLiterally("-LRB-","(").replaceAllLiterally("-RRB-",")").replaceAllLiterally("""\/""","/")).toArray
        } else {
            tokenized.map(x => "")
        }
        val ner = Corpus.splitOnNewline(fromFile(options('ner)).getLines).toArray
        logger(1, "tokenized.size = "+tokenized.size.toInt)
        logger(1, "snt.size = "+notTokenized.size.toInt)
        logger(1, "dependencies.size = "+dependencies.size.toInt)
        logger(1, "ner.size = "+ner.size.toInt)
        assert(ner.size == tokenized.size && dependencies.size == tokenized.size && notTokenized.size == tokenized.size, "Input file sizes (dependencies, ner, and tokenized files) do not match")
        logger(1, "done")

        // see http://stackoverflow.com/questions/9632094/zip-multiple-sequences for this trick
        val inputs = (0 until tokenized.size) map { i => new Input(None, tokenized(i).split(" "), notTokenized(i).split(" "), dependencies(i), ner(i)) }
        return inputs.toArray
    }

    def Input(amrdata: AMRTrainingData, input: Input, oracle: Boolean, clearUnalignedNodes: Boolean = true) : Input = { 
        new Input(
            Some(if (oracle) {
                amrdata.toOracleGraph(clearUnalignedNodes)
            } else {
            amrdata.toInputGraph
            }),
            input.sentence, input.notTokenized, input.dependencies, input.pos, input.ner)
    }

}

