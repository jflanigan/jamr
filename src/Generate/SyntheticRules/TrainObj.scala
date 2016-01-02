package edu.cmu.lti.nlp.amr.Generate.SyntheticRules
import edu.cmu.lti.nlp.amr
import edu.cmu.lti.nlp.amr.Annotation
import edu.cmu.lti.nlp.amr.logger
import edu.cmu.lti.nlp.amr.Generate._
import edu.cmu.lti.nlp.amr.Train._
import edu.cmu.lti.nlp.amr.BasicFeatureVector._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}
import scala.io.Source.fromFile

class TrainObj(val options : Map[Symbol, String]) extends edu.cmu.lti.nlp.amr.Train.TrainObj[FeatureVector](options) {

    // this implementation is not thread safe
    def zeroVector : FeatureVector = { FeatureVector() }

    val inputAMR: Array[amr.Input] = amr.Input.loadInputfiles(options)                     // TODO: we only need POS tags
    val pos: Array[Annotation[String]] = inputAMR.map(x => x.pos)
    val ruleInventory: RuleInventory = new RuleInventory(dropSenses = options.contains('dropSenseTags))
    if (options.contains('ruleInventory)) {
        ruleInventory.load(options('ruleInventory))
    } else if (options.contains('trainingData)) {
        ruleInventory.extractFromCorpus(fromFile(options('trainingData)).getLines, pos)
    } else {
        System.err.println("Error: please specify --training-data"); sys.exit(1)
    }
    val training: Array[(Rule, Input)] = ruleInventory.trainingData(fromFile(options('trainingData)).getLines, pos).filter(x => goodExample(x._1))
    def trainingSize = training.size

    val decoder = new Decoder(ruleInventory)

    var optimizer = options.getOrElse('trainingOptimizer, "Adagrad") match {     // TODO: this should go back into Train/TrainObj
        case "SSGD" => new SSGD()
        case "Adagrad" => new Adagrad()
        case x => { System.err.println("Error: unknown training optimizer " + x); sys.exit(1) }
    }

    def goodExample(rule: Rule) : Boolean = {
        // Returns true if it's a rule we should use as a training example (used to filter the rules)
        return rule.args.exists(x => !x.startsWith(":op")) && rule.args.distinct.size > 1 && rule.args.size < 6
    }

    def decode(i: Int, weights: FeatureVector) : (FeatureVector, Double, String) = {
        decoder.weights = weights
        val (rule, input) = training(i)
        logger(0, "-- Prediction --")
        val result = decoder.decode(rule.concept.realization, rule.args, input, 1).head
        logger(0, "Result:            "+result.rule)
        (result.features, result.score, "")
    }

    def oracle(i: Int, weights: FeatureVector) : (FeatureVector, Double) = {
        decoder.weights = weights
        val (rule, input) = training(i)
        logger(0, "-- Oracle --")
        logger(0, "Oracle input:      "+rule)
        val features = decoder.oracle(rule, input)
        (features, weights.dot(features))
    }

    def costAugmented(i: Int, weights: FeatureVector, scale: Double) : (FeatureVector, Double) = {
/*        decoder.features.weights = weights
        val amrData = AMRTrainingData(training(i), lowercase)
        val oracleInput : Input = Input(amrData, input(i), i, oracle = true, clearUnalignedNodes = true)
        if (!options.contains('precRecallTradeoff)) { System.err.println("Error: must specify --training-prec-recall for this training loss function."); sys.exit(1) }
        val result = decoder.decode(input(i), costFunction(oracleInput, scale, options('precRecallTradeoff).toDouble))
        return (result.features, result.score) */
        return (zeroVector, 0.0)
    }

    def train {
        train(FeatureVector())
    }

    def evalDev(options: Map[Symbol, String], pass: Int, weights: FeatureVector) {
/*        if (options.contains('trainingDev)) {
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
            val amrData = AMRTrainingData(block, lowercase)
            val oracleResult = oracle.decode(Input(amrData, input(i), i, oracle = true, clearUnalignedNodes = true))  // TODO: check clearUnalignedNodes in AMRParser line 233

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
        } */
    }

}

