package edu.cmu.lti.nlp.amr.ConceptInvoke
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.Train._
import edu.cmu.lti.nlp.amr.BasicFeatureVector._

import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable => m, immutable => i}
import scala.io.Source.fromFile

class TrainObj(val options : m.Map[Symbol, String]) extends edu.cmu.lti.nlp.amr.Train.TrainObj[FeatureVector](options) {

    // TODO: this implementation is not thread safe
    val decoder = Decoder(options, oracle = false)
    val oracle : Decoder = Decoder(options, oracle = true)
    //costAugDecoder.features.weights = weights
    def zeroVector : FeatureVector = { FeatureVector() }

    val input: Array[Input] = Input.loadInputfiles(options)
    val training: Array[String] = Corpus.getAMRBlocks(Source.stdin.getLines()).toArray
    def trainingSize = training.size

    var optimizer = options.getOrElse('trainingOptimizer, "Adagrad") match {     // TODO: this should go back into Train/TrainObj
        case "SSGD" => new SSGD()
        case "Adagrad" => new Adagrad()
        case x => { System.err.println("Error: unknown training optimizer " + x); sys.exit(1) }
    }

    def decode(i: Int, weights: FeatureVector) : (FeatureVector, Double, String) = {
        decoder.features.weights = weights
        val result = decoder.decode(input(i), Some(i))
        return (result.features, result.score, "")
    }

    def oracle(i: Int, weights: FeatureVector) : (FeatureVector, Double) = {
        oracle.features.weights = weights
        val amrData = AMRTrainingData(training(i))
        val result = oracle.decode(Input(amrData, input(i), i, oracle = true, clearUnalignedNodes = true), Some(i))
        return (result.features, result.score)
    }

    def costAugmented(i: Int, weights: FeatureVector, scale: Double) : (FeatureVector, Double) = {
        //logger(1, "costAugmented weights = "+weights.toString)
        decoder.features.weights = weights
        val amrData = AMRTrainingData(training(i))
        val oracleInput : Input = Input(amrData, input(i), i, oracle = true, clearUnalignedNodes = true)
        val result = costAugmented(input(i), oracleInput, Some(i), scale)
        return (result.features, result.score)
    }

    def costAugmented(input: Input, oracleInput: Input, trainingIndex: Option[Int], scale: Double) : DecoderResult = {    // TODO: move this into a cost augmented decoder object? Yes
        return decoder.decode(input, trainingIndex, costFunction(oracleInput, scale, options.getOrElse('trainingPrecRecallTradeoff, ".5").toDouble))
    }

    private def costFunction(oracle: Input, scale: Double, prec: Double) : (Input, PhraseConceptPair, Int, Int, List[PhraseConceptPair]) => Double = {
        assert(oracle.graph != None, "CostAugmented decoder was not passed an oracle graph")
        val oracleGraph = oracle.graph.get
        def spanStart(node: Node) : Int = {
            oracleGraph.spans(node.spans(0)).start
        }
        def spanEnd(node: Node) : Int = {
            oracleGraph.spans(node.spans(0)).end
        }
        def normalizeFrag(frag: String) : String = {
            frag.replaceAll("""^[^ ]+ :name \(name """, "(entity ")
        }
        val oracleConcepts : i.Set[(Int, Int, String)] = oracleGraph.nodes.map(x => (spanStart(x), spanEnd(x), x.concept)).toSet
        val oracleSpans : i.Map[(Int,Int), String] = oracleGraph.nodes.filter(_.spans.size != 0).map(x => {
            val span = oracleGraph.spans(x.spans(0))
            ((span.start, span.end), normalizeFrag(span.amr.toString))
            }).toMap
        val oracleStart : i.Map[Int, (Int, String)] = oracleSpans.map(x => (x._1._1, (x._1._2, x._2)))
        val oracleEdges : i.Set[(Int, Int, String, String, String)] = oracleGraph.edges.map(x => (spanStart(x._1), spanEnd(x._1), x._1.concept, x._2, x._3.concept)).toSet // x._1.spans(0) = x._3.spans(0) because in same fragment
        val oracleEdges1 : i.Set[(Int, Int, String, String)] = oracleGraph.edges.map(x => (spanStart(x._1), spanEnd(x._1), x._1.concept, x._2)).toSet
        val oracleEdges2 : i.Set[(Int, Int, String, String)] = oracleGraph.edges.map(x => (spanStart(x._1), spanEnd(x._1), x._2, x._3.concept)).toSet

        //def costFunc(input: Input, concept: PhraseConceptPair, start: Int, stop: Int) : Double = {
        val costFunc1 = (input: Input, concept: PhraseConceptPair, start: Int, stop: Int, conceptList: List[PhraseConceptPair]) => {
            val graphFrag = Graph.parse(concept.graphFrag)
            var cost : Double = 0
            if (!oracleSpans.contains(start, stop)) {
                // Predicting a fragment that shouldn't be there
                // Everything in the fragment will be wrong (concepts and edges), and there will be an incorrect edge to the fragment as well
                // This is a precision type error
                cost += prec * graphFrag.nodes.size
                cost += prec * graphFrag.edges.size
                cost += prec
            } else {
                // Predicting a fragment that should be there
                // Missing a fragment is (mostly) a recall type error
                // 1 point for every correct concept
                cost -= (1 - prec) * graphFrag.nodes.filter(x => oracleConcepts.contains(start, stop, x.concept)).size
                // 1 point for every correct (internal) edge
                cost -= (1 - prec) * graphFrag.edges.filter(x =>
                        oracleEdges1.contains((start, stop, x._1.concept, x._2))).size              // source concept matches
                cost -= (1 - prec) * graphFrag.edges.filter(x =>
                        oracleEdges2.contains((start, stop, x._2, x._3.concept))).size              // dest concept matches
                cost += (1 - prec) * graphFrag.edges.filter(x =>
                        oracleEdges.contains((start, stop, x._1.concept,x._2,x._3.concept))).size   // adjust for overlap

                // 3 points for producing something (because otherwise we could make three errors to connect the graph: two missing edges and one incorrect edge)
                cost -= 2 * (1 - prec)  // 2 missing edges (recall error)
                cost -= prec            // 1 incorrect edge (precision error)
            }
            cost * scale
        }

        val costFunc2 = (input: Input, concept: PhraseConceptPair, start: Int, stop: Int, conceptList: List[PhraseConceptPair]) => {
            val graphFrag = normalizeFrag(concept.graphFrag)
            var cost : Double = 0
            if (!oracleSpans.contains((start, stop)) || oracleSpans(start, stop) != graphFrag) {
                // Predicting a fragment that shouldn't be there
                // This is a precision type error
                cost += prec
                if (oracleStart.contains(start)) {
                    // We are also missing the start of another span, which is a recall-type error
                    //cost += (1 - prec)    // we subtracted this below (same as adding a constant to argmax)
                }
            } else {
                // Predicting a fragment that should be there
                // Correct prediction, so cost = 0
                // Missing this is a recall error, so subtract it here
                cost -= (1 - prec)
            }
            cost * scale
        }

        val costFunc3 = (input: Input, concept: PhraseConceptPair, start: Int, stop: Int, conceptList: List[PhraseConceptPair]) => {
            val graphFrag = normalizeFrag(concept.graphFrag)
            var cost : Double = 0
            if (!oracleSpans.contains((start, stop)) || oracleSpans(start, stop) != graphFrag) {
                if (oracleSpans.contains((start, stop)) && conceptList.exists(x => normalizeFrag(x.graphFrag) == oracleSpans(start, stop))) {
                    // There should be a fragment here, but we're predicting it wrong,
                    // and there is a correct fragment in our list
                    // This is a precision type error
                    cost += prec
                } else if (oracleSpans.contains((start, stop))) {
                    // There should be a fragment here, but there isn't a correct fragment in our list
                    // of possible fragments, so treat it like it's a correct answer
                    cost -= (1 - prec)
                } else {
                    // There should be no fragment here
                    // This is a precision type error
                    cost += prec
                }
                if (oracleStart.contains(start)) {
                    // We are also missing the start of another span, which is a recall-type error
                    //cost += (1 - prec)    // we subtracted this below (same as adding a constant to argmax)
                }
            } else {
                // Predicting a fragment that should be there
                // Correct prediction, so cost = 0
                // Missing this is a recall error, so subtract it here
                cost -= (1 - prec)
            }
            cost * scale
        }

    /******* Improvements to cost function to do *********
    - If it has the correct named entity type, use that as the gold standard? (check if this helps)
    - If it has no correct tag, but has the correct entity type (event, etc), then use that
    - Else if it has the correct lemma, then use that (maybe this won't help)
    - Make these graded, and try ramp1 loss, with regularizer

    ******************************************************/

        return costFunc3
    }

    def train {
        train(FeatureVector())
    }

    def evalDev(options: m.Map[Symbol, String], pass: Int, weights: FeatureVector) {
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
                                                        i), trainingIndex = None)
            val amrData = AMRTrainingData(block)
            val oracleGraph = (new Input(amrData, dependencies(i), oracle = true, index = i)).graph.get

            for (span <- stage1Result.graph.spans) {
                if (oracleGraph.spans.count(x => x.start == span.start && x.end == span.end && x.amr.toString == span.amr.toString) > 0) {
                    spanF1.correct += 1
                } else {
                    if (oracleGraph.spans.count(x => x.start == span.start && x.end == span.end) > 0) {
                        file.println("Incorrect span: "+span.words+" => "+span.amr)
                    } else {
                        file.println("Extra span: "+span.words+" => "+span.amr)
                    }
                }
            }
            for (span <- oracleGraph.spans) {
                if (stage1Result.graph.spans.count(x => x.start == span.start && x.end == span.end && x.amr.toString == span.amr.toString) == 0) {
                    file.println("Missing span: "+span.words+" => "+span.amr)
                }
            }
            spanF1.predicted += stage1Result.graph.spans.size
            spanF1.total += oracleGraph.spans.size
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

