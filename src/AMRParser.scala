package edu.cmu.lti.nlp.amr

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

import edu.cmu.lti.nlp.amr.GraphDecoder._
import edu.cmu.lti.nlp.amr.ConceptInvoke.PhraseConceptPair

/****************************** Driver Program *****************************/
object AMRParser {

    val usage = """Usage:
    // TODO: remove --tok so that the parser calls the tokenizer
scala -classpath . edu.cmu.lti.nlp.amr.AMRParser --stage1-decode --stage1-weights weights --concept-table concepts --ner namedEntities --tok tokenized.txt < inputFile
scala -classpath . edu.cmu.lti.nlp.amr.AMRParser --stage2-train -l labelset < trainfile > output_weights
scala -classpath . edu.cmu.lti.nlp.amr.AMRParser --stage2-decode -w weights -l labelset < input > output"""
//TODO: tagset option
    type OptionMap = Map[Symbol, String]

    def parseOptions(map : OptionMap, list: List[String]) : OptionMap = {
        def isSwitch(s : String) = (s(0) == '-')
        list match {
            case Nil => map
            case "--stage1-only" :: l =>                 parseOptions(map + ('stage1Only -> "true"), l)
            case "--stage1-oracle" :: l =>               parseOptions(map + ('stage1Oracle -> "true"), l)
            case "--stage1-train" :: l =>                parseOptions(map + ('stage1Train -> "true"), l)
            case "--stage1-eval" :: l =>                 parseOptions(map + ('stage1Eval -> "true"), l)
            case "--stage1-features" :: value :: l =>    parseOptions(map + ('stage1Features -> value), l)
            case "--stage1-weights" :: value :: l =>     parseOptions(map + ('stage1Weights -> value), l)
            case "--stage1-concept-table" :: v :: l =>   parseOptions(map + ('stage1ConceptTable -> v), l)
            case "--stage2-decoder" :: value :: l =>     parseOptions(map + ('stage2Decoder -> value), l)
            case "--stage2-train" :: l =>                parseOptions(map + ('stage2Train -> "true"), l)
            case "--stage2-features" :: value :: l =>    parseOptions(map + ('stage2Features -> value), l)
            case "--stage2-weights" :: value :: l =>     parseOptions(map + ('stage2Weights -> value), l)
            case "--stage2-labelset" :: value :: l =>    parseOptions(map + ('stage2Labelset -> value), l)
            case "--stage2-not-connected" :: l =>        parseOptions(map + ('stage2NotConnected -> "true"), l)
            case "--training-loss" :: value :: l =>      parseOptions(map + ('trainingLoss -> value), l)
            case "--training-cost-scale" :: value :: l =>parseOptions(map + ('trainingCostScale -> value), l)
            case "--training-optimizer" :: value :: l => parseOptions(map + ('trainingOptimizer -> value), l)
            case "--training-stepsize" :: value :: l =>  parseOptions(map + ('trainingStepsize -> value), l)
            case "--training-passes" :: value :: l =>    parseOptions(map + ('trainingPasses -> value), l)
            case "--training-data" :: value :: tail =>   parseOptions(map + ('trainingData -> value), tail) // used to be "--amr-oracle-data"
            case "--output-format" :: value :: l =>      parseOptions(map + ('outputFormat -> value), l)
            //case "--amr-oracle-data" :: value :: tail => parseOptions(map + ('amrOracleData -> value), tail)
            case "--dependencies" :: value :: tail =>    parseOptions(map + ('dependencies -> value), tail)
            case "--ner" :: value :: tail =>             parseOptions(map + ('ner -> value), tail)
            case "--snt" :: value :: tail =>             parseOptions(map ++ Map('notTokenized -> value), tail)
            case "--tok" :: value :: tail =>             parseOptions(map ++ Map('tokenized -> value), tail)
            case "-v" :: value :: tail =>                parseOptions(map ++ Map('verbosity -> value), tail)

            case string :: opt2 :: tail if isSwitch(opt2) => parseOptions(map ++ Map('infile -> string), list.tail)
            case string :: Nil =>  parseOptions(map ++ Map('infile -> string), list.tail)
            case option :: tail => println("Error: Unknown option "+option) 
                                   sys.exit(1)
      }
    }

    def time[A](a: => A) = {
       val now = System.nanoTime
       val result = a
       val micros = (System.nanoTime - now) / 1000
       System.err.println("Decoded in %,d microseconds".format(micros))
       result
    }

    def main(args: Array[String]) {

        if (args.length == 0) { println(usage); sys.exit(1) }
        val options = parseOptions(Map(),args.toList)

        verbosity = options.getOrElse('verbosity, "0").toInt

        val outputFormat = options.getOrElse('outputFormat,"triples").split(",").toList

        val stage1 : ConceptInvoke.Decoder = {
            if (!options.contains('stage1Oracle) && !options.contains('stage2Train)) {
                ConceptInvoke.Decoder(options, oracle = false)
            } else {
                assert(!options.contains('stage1Train), "Error: --stage1-oracle should not be specified with --stage1-train")
                ConceptInvoke.Decoder(options, oracle = true)
            }
        }

        val stage2 : Option[GraphDecoder.Decoder] = {
            if((options.contains('stage1Only) || options.contains('stage1Train)) && !options.contains('stage2Train)) {
                None
            } else {
                Some(GraphDecoder.Decoder(options))
            }
        }

        val stage2Oracle : Option[GraphDecoder.Decoder] = {
            if(options.contains('trainingData) || options.contains('stage2Train)) {
                Some(new GraphDecoder.Oracle(GraphDecoder.getFeatures(options)))
            } else {
                None
            }
        }

        if (options.contains('stage1Train) || options.contains('stage2Train)) {

            ////////////////// Training  ////////////////

            if (options.contains('stage1Train) && options.contains('stage2Train)) {
                System.err.println("Error: please specify either stage1 training or stage2 training (not both)")
                sys.exit(1)
            }

            if (options.contains('stage1Train)) {

                val stage1 = new ConceptInvoke.TrainObj(options)
                stage1.train

            }

            if (options.contains('stage2Train)) {

                val stage2 = new GraphDecoder.TrainObj(options)
                stage2.train

            }

        } else {

            /////////////////// Decoding /////////////////

            if (!options.contains('stage1Weights)) {
                System.err.println("Error: No stage1 weights file specified"); sys.exit(1)
            }
            stage1.features.weights.read(Source.fromFile(options('stage1Weights).asInstanceOf[String]).getLines())

            //logger(0, "Stage1 weights:\n"+stage1.features.weights.toString)

            if (!options.contains('stage2Weights)) {
                System.err.println("Error: No stage2 weights file specified")
                sys.exit(1)
            }
            val stage2weightfile : String = options('stage2Weights)

            logger(0, "Reading weights")
            if (stage2 != None) {
                stage2.get.features.weights.read(Source.fromFile(stage2weightfile).getLines())
                if (stage2Oracle != None) {
                    stage2Oracle.get.features.weights.read(Source.fromFile(stage2weightfile).getLines())
                }
            }
            logger(0, "done")

            val input = stdin.getLines.toArray
            val tokenized = fromFile(options('tokenized).asInstanceOf[String]).getLines.toArray
            val nerFile = Corpus.splitOnNewline(fromFile(options('ner).asInstanceOf[String]).getLines).toArray

            val dependencies: Array[String] = if (options.contains('dependencies)) {
                (for {
                    block <- Corpus.splitOnNewline(Source.fromFile(options('dependencies).asInstanceOf[String]).getLines())
                } yield block.replaceAllLiterally("-LRB-","(").replaceAllLiterally("-RRB-",")").replaceAllLiterally("""\/""","/")).toArray
            } else {
                new Array(0)
            }

            case class F1(var correct: Double, var predicted: Double, var total: Double) {
                def precision : Double = correct / predicted
                def recall : Double = correct / total
                def f1 : Double = 2 * (precision * recall) / (precision + recall)
                override def toString : String = { "Precision: "+precision.toString+"\nRecall: "+recall.toString+"\nF1: "+f1.toString }
            }
            val spanF1 = F1(0,0,0)

            for ((block, i) <- Corpus.splitOnNewline(fromFile(options('trainingData)).getLines).filter(_.matches("(.|\n)*\n\\((.|\n)*"))/*.take(877)*/.zipWithIndex) {
            time {
                val line = input(i)
                logger(0, "Sentence:\n"+line+"\n")
                val tok = tokenized(i)
                val ner = nerFile(i)
                val inputGraph = if (options.contains('stage1Oracle)) { Some(AMRTrainingData(block).toInputGraph) } else { None }
                val stage1Result = stage1.decode(new Input(inputGraph,
                                                           tok.split(" "),
                                                           line.split(" "),
                                                           dependencies(i),
                                                           ner))
                logger(1, "Concepts:")
                for ((id, node) <- stage1Result.graph.getNodeById) {
                    logger(1, "id = "+id+" concept = "+node.concept)
                }
                logger(0, "Spans:")
                for ((span, i) <- stage1Result.graph.spans.sortBy(x => x.words.toLowerCase).zipWithIndex) {
                    logger(0, "Span "+(i+1).toString+":  "+span.words+" => "+span.amr)
                }
                logger(0, "")

                stage1Result.graph.normalizeInverseRelations
                stage1Result.graph.addVariableToSpans


                //val amrdata = AMRTrainingData(block)
                val amrdata2 = AMRTrainingData(block)   // 2nd copy for oracle
                logger(1, "Node.spans:")
                for (node <- amrdata2.graph.nodes) {
                    logger(1, node.concept+" "+node.spans.toList)
                }

                var decoderResult = stage1Result

                    // TODO: clean up this code

                if (!options.contains('stage1Only)) {
                    val decoder = stage2.get
                    decoderResult = decoder.decode(new Input(stage1Result.graph,
                                                             tok.split(" "),
                                                             dependencies(i)))
                }//endif (!options.contains('stage1Only))

                if (false && options.contains('trainingData)) {
                    val oracle = stage2Oracle.get
                    val oracleResult = oracle.decode(new Input(amrdata2, dependencies(i), oracle = true))
                    logger(0, "\nOracle Spans:")
                    for ((span, i) <- amrdata2.graph.spans.sortBy(x => x.words.toLowerCase).zipWithIndex) {
                        logger(0, "Span "+(i+1).toString+":  "+span.words+" => "+span.amr)
                    }
                    logger(0, "")
                    if (options.contains('stage1Eval)) {
                        for (span <- stage1Result.graph.spans) {
                            if (oracleResult.graph.spans.count(x => x.start == span.start && x.end == span.end /*&& x.amr.prettyString(detail = 0, pretty = false).replaceAll("""\([^ ]* :name ""","") == span.amr.prettyString(detail = 0, pretty = false).replaceAll("""\([^ ]* :name ""","")*/) > 0) {
                                spanF1.correct += 1
                            }
                        }
                        spanF1.predicted += stage1Result.graph.spans.size
                        spanF1.total += oracleResult.graph.spans.size
                    }
                    logger(0, "Dependencies:\n"+dependencies(i)+"\n")
                    logger(0, "Oracle:\n"+oracleResult.graph.printTriples(detail = 1, extra = (node1, node2, relation) => {
                        "\t"+oracle.features.ffDependencyPathv2(node1, node2, relation).toString.split("\n").filter(_.matches("^C1.*")).toList.toString+"\t"+oracle.features.localScore(node1, node2, relation).toString
                        //"\n"+oracle.features.ffDependencyPathv2(node1, node2, relation).toString.split("\n").filter(_.matches("^C1.*")).toList.toString+"\nScore = "+decoder.features.localScore(node1, node2, relation).toString+"  Relevent weights:\n"+decoder.features.weights.slice(decoder.features.localFeatures(node1, node2, relation)).toString
                    })+"\n")
                }//endif (options.contains('amrOracleData))

                if (!options.contains('stage1Only)) {
                    val decoder = stage2.get
                    logger(0, decoder.features.input)
                    logger(0, "AMR:\n"+decoderResult.graph.printTriples(detail = 1, extra = (node1, node2, relation) => {
                        "\t"+decoder.features.ffDependencyPathv2(node1, node2, relation).toString.split("\n").filter(_.matches("^C1.*")).toList.toString+"\t"+decoder.features.localScore(node1, node2, relation).toString
                        //"\n"+decoder.features.ffDependencyPathv2(node1, node2, relation).toString.split("\n").filter(_.matches("^C1.*")).toList.toString+"\nScore = "+decoder.features.localScore(node1, node2, relation).toString+"  Relevent weights:\n"+decoder.features.weights.slice(decoder.features.localFeatures(node1, node2, relation)).toString
                    })+"\n")
                }

                if (outputFormat.contains("AMR")) {
                    println(decoderResult.graph.root.prettyString(detail=1, pretty=true) + '\n')
                }
                if (outputFormat.contains("triples")) {
                    println(decoderResult.graph.printTriples(detail = 1)+"\n")
                }
            }
            }
            
            if (options.contains('stage1Eval)) {
                logger(0, "--- Stage1 evaluation ---\n"+spanF1.toString)
            }
        }
    }
}

