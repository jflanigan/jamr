package edu.cmu.lti.nlp.amr.GraphDecoder
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.Train._
import edu.cmu.lti.nlp.amr.FastFeatureVector._

import java.io.StringWriter
import java.io.PrintWriter
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
import scala.util.parsing.combinator._
import scala.sys.process._

class TrainObj(val options : Map[Symbol, String]) extends edu.cmu.lti.nlp.amr.Train.TrainObj[FeatureVector](options) {

    //val decoder = Decoder(options)
    //val oracle = new Oracle(getFeatures(options))
    //val costAug = new CostAugmented(Decoder(options), options.getOrElse('trainingCostScale,"10.0").toDouble)
    //val weights = decoder.features.weights
    //oracle.features.weights = weights
    //costAug.features.weights = weights

    def zeroVector : FeatureVector = { new FeatureVector(getLabelset(options).map(_._1)) }

    val input: Array[Input] = Input.loadInputfiles(options)
    val training: Array[String] = Corpus.getAMRBlocks(Source.stdin.getLines()).toArray
    def trainingSize = training.size

    val stage1 = if (options.contains('stage2TrainPredictedConcepts)) {
        Some(ConceptInvoke.Decoder(options, oracle = false))
    } else {
        None
    }

    var optimizer = options.getOrElse('trainingOptimizer, "Adagrad") match {     // TODO: this should go back into Train/TrainObj
        case "SSGD" => new SSGD()
        case "Adagrad" => new Adagrad()
        case x => { System.err.println("Error: unknown training optimizer " + x); sys.exit(1) }
    }

    val outputFormat = options.getOrElse('outputFormat,"triples").split(",").toList

    def getInput(i: Int) : Input = {
        val amrdata = AMRTrainingData(training(i))
        if (options.contains('stage2TrainPredictedConcepts)) {
            // Use stage1 output instead of the gold concepts during training
            val in = input(i)
            val stage1Result = stage1.get.decode(in, Some(i))   // TODO: check if stage1-training-leave-one-out
            return new Input(Some(stage1Result.graph), in.sentence, in.notTokenized, in.dependencies, in.pos, in.ner, i)
        } else {
            return Input(amrdata, input(i), i, oracle = false)
        }
    }

    def decode(i: Int, weights: FeatureVector) : (FeatureVector, Double, String) = {
        val decoder = Decoder(options)
        decoder.features.weights = weights
        val amrdata = AMRTrainingData(training(i))
        logger(0, "Sentence:\n"+amrdata.sentence.mkString(" ")+"\n")
        val result = decoder.decode(getInput(i))
        logger(0, "Spans:")
        for ((span, i) <- result.graph.spans.zipWithIndex) {
            logger(0, "Span "+(i+1).toString+":  "+span.words+" => "+span.amr)
        }
        logger(0, "AMR:")
        if (outputFormat.contains("AMR")) {
            logger(0, result.graph.prettyString(detail = 1, pretty = true)+"\n")
        }
        if (outputFormat.contains("triples")) {
            //logger(0, result.graph.printTriples(detail = 1)+"\n")
            logger(0, result.graph.printTriples(
                detail = 1,
                extra = (node1, node2, relation) => { // TODO: put back in
                    "" //"\t"+decoder.features.ffDependencyPathv2(node1, node2, relation).toString.split("\n").filter(_.matches("^C1.*")).toList.toString+"\t"+decoder.features.localScore(node1, node2, relation).toString
                })+"\n")
        }
        //logger(1, "Decoder features:\n"+result.features+"\n")
        return (result.features, result.score, if (outputFormat.contains("AMR")) { result.graph.prettyString(detail = 1, pretty = true) } else { "" })
    }

    def oracle(i: Int, weights: FeatureVector) : (FeatureVector, Double) = {
        val oracle = Oracle(options)
        oracle.features.weights = weights
        val amrdata = AMRTrainingData(training(i))
        val result = oracle.decode(Input(amrdata, input(i), i, oracle = true))

        logger(0, "Oracle:")
        if (outputFormat.contains("AMR")) {
           val result2 = oracle.decode(Input(amrdata, input(i), i, oracle = true, clearUnalignedNodes = false))
           logger(0, result2.graph.prettyString(detail = 1, pretty = true)+"\n")
        }
        if (outputFormat.contains("triples")) {
           //logger(0, result.graph.printTriples(detail = 1)+"\n")
           logger(0, result.graph.printTriples(
                detail = 1,
                extra = (node1, node2, relation) => {
                    "" //"\t"+oracle.features.ffDependencyPathv2(node1, node2, relation).toString.split("\n").filter(_.matches("^C1.*")).toList.toString+"\t"+decoder.features.localScore(node1, node2, relation).toString
                })+"\n")
        }
        //logger(0, "Dependencies:\n"+dependencies(i)+"\n")
        logger(1, "Oracle features:\n"+result.features+"\n")
        return (result.features, result.score)
    }

    def costAugmented(i: Int, weights: FeatureVector, scale: Double) : (FeatureVector, Double) = {
        if (scale >= 0) {
            logger(0, "CostAug Prediction")
        } else { 
            logger(0, "CostAug Oracle")
        }
        val costAug = if (scale >= 0 || !options.contains('trainingStage2OracleDecoder)) {
            val decoder = Decoder(options)
            decoder.features.weights = weights // this is not needed since CostAugmented.features = decoder.features (CostAugmented class sets it this way)
            new CostAugmented(decoder, scale, options.getOrElse('trainingPrecRecallTradeoff,"0.5").toDouble, options)
        } else {
            val decoder_save = options('stage2Decoder)
            options('stage2Decoder) = options('trainingStage2OracleDecoder)
            val decoder = Decoder(options)
            decoder.features.weights = weights // this is not needed since CostAugmented.features = decoder.features (CostAugmented class sets it this way)
            val costAug = new CostAugmented(decoder, scale, options.getOrElse('trainingPrecRecallTradeoff,"0.5").toDouble, options)
            options('stage2Decoder) = decoder_save
            costAug
        }
        costAug.features.weights = weights

        val amrdata1 = AMRTrainingData(training(i))
        logger(0, "Sentence:\n"+amrdata1.sentence.mkString(" ")+"\n")
        val result = costAug.decode(Input(amrdata1, input(i), i, oracle = true),
                                    if (options.contains('stage2TrainPredictedConcepts)) { getInput(i).graph } else { None } )
        logger(0, "Spans:")
        for ((span, i) <- amrdata1.graph.spans.zipWithIndex) {
            logger(0, "Span "+(i+1).toString+":  "+span.words+" => "+span.amr)
        }
        logger(0, "AMR:")
        if (outputFormat.contains("AMR")) {
            logger(0, result.graph.prettyString(detail = 1, pretty = true)+"\n")
        }
        if (outputFormat.contains("triples")) {
            //logger(0, result.graph.printTriples(detail = 1)+"\n")
            logger(0, result.graph.printTriples(
                detail = 1,
                extra = (node1, node2, relation) => {
                    "" //"\t"+costAug.features.ffDependencyPathv2(node1, node2, relation).toString.split("\n").filter(_.matches("^C1.*")).toList.toString+"\t"+costAug.features.localScore(node1, node2, relation).toString
                })+"\n")
        }
        logger(1, "Decoder features:\n"+result.features+"\n")
        return (result.features, result.score)
    }

    def train {
        val initialWeights = FeatureVector(getLabelset(options).map(x => x._1))
        if (options.contains('trainingInitialWeights)) {
            initialWeights.read(Source.fromFile(options('trainingInitialWeights)).getLines)
        }
        train(initialWeights)
    }

    def evalDev(options: Map[Symbol, String], pass: Int, weights: FeatureVector) {
        val devDecode = options('trainingOutputFile)+".iter"+pass.toString+".decode_dev"
        val dev = options('trainingDev) // assumes .aligned, .aligned.no_opN, .snt, .tok, .snt.deps, .snt.IllinoisNER

        val snt = fromFile(dev+".aligned.no_opN").getLines.toArray // aka 'input' in AMRParser decode
        val tokenized = fromFile(dev+".snt.tok").getLines.toArray
        val nerFile = Corpus.splitOnNewline(fromFile(dev+".snt.IllinoisNER").getLines).toArray
        val dependencies = Corpus.splitOnNewline(fromFile(dev+".snt.deps").getLines).map(block => block.replaceAllLiterally("-LRB-","(").replaceAllLiterally("-RRB-",")").replaceAllLiterally("""\/""","/")).toArray

        val file = new java.io.PrintWriter(new java.io.File(devDecode), "UTF-8")
        for { (block, i) <- Corpus.splitOnNewline(fromFile(dev+".aligned.no_opN").getLines).zipWithIndex
                if block.split("\n").exists(_.startsWith("(")) } { // needs to contain some AMR
            try {
                val inputGraph = AMRTrainingData(block).toInputGraph
                val stage2Alg_Save = options('stage2Decoder)
                val stage2 = if (stage2Alg_Save == "Alg1") {
                        options('stage2Decoder) = "Alg1a"
                        GraphDecoder.Decoder(options)
                    } else {
                        GraphDecoder.Decoder(options)
                    }
                options('stage2Decoder) = stage2Alg_Save
                stage2.features.weights = weights
                val decoderResult = stage2.decode(new Input(inputGraph, tokenized(i).split(" "), dependencies(i), i))
                file.println(decoderResult.graph.prettyString(detail=1, pretty=true) + '\n')
            } catch {
                case e : Throwable => if (options.contains('ignoreParserErrors)) {
                    file.println("# THERE WAS AN EXCEPTION IN THE PARSER.  Returning an empty graph.")
                    if (options.contains('printStackTraceOnErrors)) {
                        val sw = new StringWriter()
                        e.printStackTrace(new PrintWriter(sw))
                        file.println(sw.toString.split("\n").map(x => "# "+x).mkString("\n"))
                    }
                    file.println(Graph.AMREmpty.prettyString(detail=1, pretty=true) + '\n')
                } else {
                    throw e
                }
            }
        }
        file.close

        try {
            val externalEval = stringToProcess("python "+options('smatchEval)+" -f "+devDecode+" "+dev+".aligned").lines.toList
            logger(0, "--- Performance on Dev ---\n" + externalEval.mkString("\n") + "\n")
        } catch {
            case _ : Throwable => 
        }
    }

/*  TODO: port back from SDP
    def f1SufficientStatistics(i: Int, weights: FeatureVector) : (Double, Double, Double) = {
        // returns (num_correct, num_predicted, num_gold)
        val decoder = Decoder(options)
        decoder.features.weights = weights
        val result = decoder.decode(Input(inputAnnotatedSentences(i), inputGraphs(i)))
        
        val oracle = new Oracle(getFeatures(options), labelset)
        oracle.features.weights = weights
        val oracleResult = oracle.decode(Input(inputAnnotatedSentences(i), oracleGraphs(i)))

        return SDPGraph.evaluate(result.graph.asInstanceOf[SDPGraph], oracleResult.graph.asInstanceOf[SDPGraph])
    } */
}

