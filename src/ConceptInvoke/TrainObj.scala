package edu.cmu.lti.nlp.amr.ConceptInvoke
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.Train._
import edu.cmu.lti.nlp.amr.BasicFeatureVector._

import java.lang.Math.abs
import java.lang.Math.log
import java.lang.Math.exp
import java.lang.Math.random
import java.lang.Math.floor
import java.lang.Math.min
import java.lang.Math.max
import scala.io.Source
import scala.io.Source.stdin
import scala.io.Source.fromFile
import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

class TrainObj(val options : Map[Symbol, String]) extends edu.cmu.lti.nlp.amr.Train.TrainObj[FeatureVector](options) {

    // TODO: this implementation is not thread safe
    val decoder = Decoder(options, oracle = false)
    val oracle : Decoder = Decoder(options, oracle = true)
    //costAugDecoder.features.weights = weights
    def zeroVector : FeatureVector = { FeatureVector() }

    var optimizer = options.getOrElse('trainingOptimizer, "Adagrad") match {     // TODO: this should go back into Train/TrainObj
        case "SSGD" => new SSGD()
        case "Adagrad" => new Adagrad()
        case x => { System.err.println("Error: unknown training optimizer " + x); sys.exit(1) }
    }

    def decode(i: Int, weights: FeatureVector) : (FeatureVector, Double, String) = {
        decoder.features.weights = weights
        val result = decoder.decode(input(i))
        return (result.features, result.score, "")
    }

    def oracle(i: Int, weights: FeatureVector) : (FeatureVector, Double) = {
        oracle.features.weights = weights
        val amrData = AMRTrainingData(training(i))
        val result = oracle.decode(Input.Input(amrData, input(i), i, oracle = true, clearUnalignedNodes = true))
        return (result.features, result.score)
    }

    def costAugmented(i: Int, weights: FeatureVector, scale: Double) : (FeatureVector, Double) = {
        assert(false, "Need to implement stage1 cost augmented decoding")
        return (decoder.decode(input(i)).features, 0.0)
    }

    def train {
        train(FeatureVector())
    }

    def evalDev(options: Map[Symbol, String], pass: Int, weights: FeatureVector) {
        if (options.contains('trainingDev)) {
        logger(-1, "Decoding dev...")
        val verbosity_save = verbosity  // TODO: could also just change the logging stream (add a var for the logging stream in amr.logger, and change it)
        verbosity = java.lang.Integer.MIN_VALUE     // Like Double.NEGATIVE_INFINITY, but for integers
        //try {
        val devDecode = options('trainingOutputFile)+".iter"+pass.toString+".decode_dev"
        val dev = options('trainingDev) // assumes .aligned, .aligned.no_opN, .snt, .tok, .snt.deps, .snt.IllinoisNER

        val snt = fromFile(dev+".snt").getLines.toArray // aka 'input' in AMRParser decode
        val tokenized = fromFile(dev+".snt.tok").getLines.toArray
        val ner = Corpus.splitOnNewline(fromFile(dev+".snt.IllinoisNER").getLines).toArray
        val dependencies = Corpus.splitOnNewline(fromFile(dev+".snt.deps").getLines).map(block => block.replaceAllLiterally("-LRB-","(").replaceAllLiterally("-RRB-",")").replaceAllLiterally("""\/""","/")).toArray

        case class F1(var correct: Double, var predicted: Double, var total: Double) {
            def precision : Double = correct / predicted
            def recall : Double = correct / total
            def f1 : Double = 2 * (precision * recall) / (precision + recall)
            override def toString : String = { "Precision: "+precision.toString+"\nRecall: "+recall.toString+"\nF1: "+f1.toString }
        }
        val spanF1 = F1(0,0,0)

        decoder.features.weights = weights

        val file = new java.io.PrintWriter(new java.io.File(devDecode), "UTF-8")
        for((block, i) <- Corpus.splitOnNewline(fromFile(dev+".aligned.no_opN").getLines).filter(x => x.split("\n").exists(_.startsWith("("))).zipWithIndex) {  // TODO: add this filter for GraphDecoder.TrainObj
            file.println("Sentence: " + snt(i))
            //file.println(decoderResult.graph.prettyString(detail=1, pretty=true) + '\n')

            val stage1Result = decoder.decode(new Input(None,
                                                        tokenized(i).split(" "),
                                                        snt(i).split(" "),
                                                        dependencies(i),
                                                        ner(i),
                                                        None))
            val amrData = AMRTrainingData(block)
            val oracleResult = oracle.decode(Input.Input(amrData, input(i), i, oracle = true, clearUnalignedNodes = true))  // TODO: check clearUnalignedNodes in AMRParser line 233

            for (span <- stage1Result.graph.spans) {
                if (oracleResult.graph.spans.count(x => x.start == span.start && x.end == span.end && x.amr.toString == span.amr.toString) > 0) {
                    spanF1.correct += 1
                } else {
                    if (oracleResult.graph.spans.count(x => x.start == span.start && x.end == span.end) > 0) {
                        file.println("Incorrect span: "+span.words+" => "+span.amr)
                    } else {
                        file.println("Extra span: "+span.words+" => "+span.amr)
                    }
                }
            }
            for (span <- oracleResult.graph.spans) {
                if (stage1Result.graph.spans.count(x => x.start == span.start && x.end == span.end && x.amr.toString == span.amr.toString) == 0) {
                    file.println("Missing span: "+span.words+" => "+span.amr)
                }
            }
            spanF1.predicted += stage1Result.graph.spans.size
            spanF1.total += oracleResult.graph.spans.size
            file.println("")
        }
        verbosity = verbosity_save
        logger(-1, "--- Performance on Dev ---\n" + spanF1.toString + "\n")
        file.println("--- Performance on Dev ---\n" + spanF1.toString)
        file.close

        /*} catch {
            case _ : Throwable =>
        } */
        }
    }

}

