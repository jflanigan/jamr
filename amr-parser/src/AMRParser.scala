package edu.cmu.lti.nlp.amr

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
import scala.io.Source.stdin
import scala.io.Source.fromFile
import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator._

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
            case "--stage1-only" :: tail =>
                      parseOptions(map ++ Map('stage1Only -> "true"), tail)
            case "--stage1-oracle" :: tail =>
                      parseOptions(map ++ Map('stage1Oracle -> "true"), tail)
            case "--stage1-train" :: tail =>
                      parseOptions(map ++ Map('stage1Train -> "true"), tail)
            case "--stage1-features" :: value :: tail =>
                      parseOptions(map ++ Map('stage1Features -> value), tail)
            case "--stage1-weights" :: value :: tail =>
                      parseOptions(map ++ Map('stage1Weights -> value), tail)
            case "--stage1-concept-table" :: value :: tail =>
                      parseOptions(map ++ Map('stage1ConceptTable -> value), tail)
            case "--stage2-decoder" :: value :: tail =>
                      parseOptions(map ++ Map('stage2Decoder -> value), tail)
            case "--stage2-train" :: tail =>
                      parseOptions(map ++ Map('stage2Train -> "true"), tail)
            case "--stage2-features" :: value :: tail =>
                      parseOptions(map ++ Map('stage2Features -> value), tail)
            case "--stage2-weights" :: value :: tail =>
                      parseOptions(map ++ Map('stage2Weights -> value), tail)
            case "--stage2-labelset" :: value :: tail =>
                      parseOptions(map ++ Map('stage2Labelset -> value), tail)
            case "--stage2-not-connected" :: tail =>
                      parseOptions(map ++ Map('stage2NotConnected -> "true"), tail)
            case "--training-loss" :: value :: tail =>
                      parseOptions(map ++ Map('trainingLoss -> value), tail)
            case "--training-optimizer" :: value :: tail =>
                      parseOptions(map ++ Map('trainingOptimizer -> value), tail)
            case "--training-stepsize" :: value :: tail =>
                      parseOptions(map ++ Map('trainingStepsize -> value), tail)
            case "--training-passes" :: value :: tail =>
                      parseOptions(map ++ Map('trainingPasses -> value), tail)
            case "--output-format" :: value :: tail =>
                      parseOptions(map ++ Map('outputFormat -> value), tail)
            case "--amr-oracle-data" :: value :: tail =>
                      parseOptions(map ++ Map('amrOracleData -> value), tail)
            case "--dependencies" :: value :: tail =>
                      parseOptions(map ++ Map('dependencies -> value), tail)
            case "--ner" :: value :: tail =>
                      parseOptions(map ++ Map('ner -> value), tail)
            case "--tok" :: value :: tail =>
                      parseOptions(map ++ Map('tokenized -> value), tail)
            case "-v" :: value :: tail =>
                      parseOptions(map ++ Map('verbosity -> value), tail)
            case string :: opt2 :: tail if isSwitch(opt2) => 
                      parseOptions(map ++ Map('infile -> string), list.tail)
            case string :: Nil =>  parseOptions(map ++ Map('infile -> string), list.tail)
            case option :: tail => println("Error: Unknown option "+option) 
                                   sys.exit(1) 
      }
    }

    def stage2Features(options: OptionMap) : List[String] = {
        options.getOrElse('stage2Features, "conceptBigram,rootConcept").split(",").toList.filter(x => x != "edgeId" && x != "labelWithId")
    }

    def initStage2(options: OptionMap) : GraphDecoder.Decoder = {
        if (!options.contains('stage2Labelset)) {
            System.err.println("Error: No labelset file specified"); sys.exit(1)
        }

        val labelset: Array[(String, Int)] = { 
            Source.fromFile(options('stage2Labelset)).getLines().toArray.map(x => {
                val split = x.split(" +")
                (split(0), if (split.size > 1) { split(1).toInt } else { 1000 })
            })
        }

        val features = stage2Features(options)
        logger(0, "features = " + features)

        val connected = !options.contains('stage2NotConnected)
        logger(0, "connected = " + connected)

        if (!options.contains('stage2Decoder)) {
            System.err.println("Error: No stage2 decoder specified"); sys.exit(1)
        }

        val decoder: Decoder = options('stage2Decoder) match {
            case "Alg1" => new Alg1(features, labelset)
            case "Alg1a" => new Alg1(features, labelset, connectedConstraint = "and")
            case "Alg2" => new Alg2(features, labelset, connected)
            case "DD" => new DualDecomposition(features, labelset, 1)
            case "LR" => new LagrangianRelaxation(features, labelset, 1, 30)
            case x => { System.err.println("Error: unknown stage2 decoder " + x); sys.exit(1) }
        }

        val outputFormat = options.getOrElse('outputFormat,"triples").split(",").toList
        if (outputFormat.contains("AMR") && !connected) {
            println("Cannot have both -stage2NotConnected flag and --outputFormat \"AMR\""); sys.exit(1)
        }

        if (options('stage2Decoder) == "Alg1" && outputFormat.contains("AMR")) {
            println("Cannot have --outputFormat \"AMR\" for stage2 Alg1 (graph may not be connected!)")
            sys.exit(1)
        }

        return decoder
    }

    def initStage1(options: OptionMap, oracle: Boolean = false) : ConceptInvoke.Decoder = {
        val stage1Features = options.getOrElse('stage1Features,"length,count").split(",").toList
        logger(0, "Stage1 features = " + stage1Features)

        if (!options.contains('stage1ConceptTable)) {
            System.err.println("Error: No concept table specified"); sys.exit(1)
        }
        val conceptFile = options('stage1ConceptTable)
        val conceptTable = Source.fromFile(conceptFile).getLines.map(x => new PhraseConceptPair(x)).toArray
        val useNER = options.contains('ner)
        if (oracle) {
            new ConceptInvoke.Oracle(stage1Features, conceptTable, useNER)
        } else {
            new ConceptInvoke.Decoder1(stage1Features, conceptTable, useNER)
        }
    }

    def main(args: Array[String]) {

        if (args.length == 0) { println(usage); sys.exit(1) }
        val options = parseOptions(Map(),args.toList)

        verbosity = options.getOrElse('verbosity, "0").toInt

        val outputFormat = options.getOrElse('outputFormat,"triples").split(",").toList

        val stage1 : ConceptInvoke.Decoder = {
            if (!options.contains('stage1Oracle)) {
                initStage1(options, oracle = false)
            } else {
                assert(!options.contains('stage1Train), "Error: --stage1-oracle should not be specified with --stage1-train")
                initStage1(options, oracle = true)
            }
        }

        val stage2 : Option[GraphDecoder.Decoder] = {
            if((options.contains('stage1Only) || options.contains('stage1Train)) && !options.contains('stage2Train)) {
                None
            } else {
                Some(initStage2(options))
            }
        }

        val stage2Oracle : Option[GraphDecoder.Decoder] = {
            if(options.contains('amrOracleData) || options.contains('stage2Train)) {
                Some(new GraphDecoder.Oracle(stage2Features(options)))
            } else {
                None
            }
        }


        if (options.contains('stage1Train) || options.contains('stage2Train)) {

                ////////////////// Training Setup ////////////////

            if (options.contains('stage1Train) && options.contains('stage2Train)) {
                System.err.println("Error: please specify either stage1 training or stage2 training (not both)")
                sys.exit(1)
            }

            Runtime.getRuntime().addShutdownHook(new Thread() {
                override def run() {
                    System.err.print("Writing out weights... ")
                    if (options.contains('stage1Train)) {
                        print(stage1.features.weights.unsorted)
                    } else {
                        print(stage2.get.features.weights.unsorted)
                    }
                    System.err.println("done")
                }
            })

            val passes = options.getOrElse('trainingPasses, "20").toInt
            val stepsize = options.getOrElse('trainingStepsize, "1.0").toDouble
            if (!options.contains('trainingOptimizer)) {
                System.err.println("Error: No training optimizer specified"); sys.exit(1)
            }
            val optimizer: Optimizer = options('trainingOptimizer).asInstanceOf[String] match {
                case "SSGD" => new SSGD()
                case "Adagrad" => new Adagrad()
                case x => { System.err.println("Error: unknown training optimizer " + x); sys.exit(1) }
            }

            System.err.print("Loading training data...")
            val training: Array[String] = (for {
                block <- Corpus.splitOnNewline(io.Source.stdin.getLines())
                if block.matches("(.|\n)*\n\\((.|\n)*")     // needs to contain some AMR
            } yield block).toArray
            val dependencies = if (options.contains('dependencies)) {
                (for {
                    block <- Corpus.splitOnNewline(Source.fromFile(options('dependencies).asInstanceOf[String]).getLines())
                } yield block.replaceAllLiterally("-LRB-","(").replaceAllLiterally("-RRB-",")").replaceAllLiterally("""\/""","/")).toArray
            } else {
                training.map(x => "")
            }
            val tokenized = fromFile(options('tokenized)).getLines.toArray
            val nerFile = Corpus.splitOnNewline(fromFile(options('ner)).getLines).toArray
            logger(0, "training.size = "+training.size.toInt)
            logger(0, "tokenized.size = "+tokenized.size.toInt)
            logger(0, "dependencies.size = "+dependencies.size.toInt)
            logger(0, "ner.size = "+nerFile.size.toInt)
            System.err.println(" done")

            if (options.contains('stage1Train)) {

                ////////////////// Stage1 Training ////////////////

                val stage1Oracle = initStage1(options, oracle = true)

                def gradient(i: Int) : FeatureVector = {
                    logger(0, "Sentence # "+i.toString)
                    logger(0, "Sentence: "+tokenized(i))
                    val snt = AMRTrainingData.getUlfString(training(i))("::snt").split(" ")
                    val input = new Input(None, tokenized(i).split(" "), snt, dependencies(i), nerFile(i))
                    val feats = stage1.decode(input).features
                    input.graph = Some(AMRTrainingData(training(i)).toOracleGraph(clearUnalignedNodes = true))
                    feats -= stage1Oracle.decode(input).features
                    logger(0, "Gradient:\n"+feats.toString)
                    return feats
                }

                optimizer.learnParameters(
                    i => gradient(i),
                    stage1.features.weights,
                    training.size,
                    passes,
                    stepsize,
                    avg = false)

            } else {

                ////////////////// Stage2 Training ////////////////

            val decoder = stage2.get
            val oracle = stage2Oracle.get

            val weights = optimizer.learnParameters(
                //i => decoder.decode(AMRTrainingData(training(i)).toInput).features,
                i => { val amrdata1 = AMRTrainingData(training(i))
                       logger(0, "Sentence:\n"+amrdata1.sentence.mkString(" ")+"\n")
                       val result1 = decoder.decode(new Input(amrdata1, dependencies(i), oracle = false))
                        logger(0, "Spans:")
                        for ((span, i) <- amrdata1.graph.spans.zipWithIndex) {
                            logger(0, "Span "+(i+1).toString+":  "+span.words+" => "+span.amr)
                        }
                       logger(0, "AMR:")
                       if (outputFormat.contains("AMR")) {
                           logger(0, result1.graph.root.prettyString(detail = 1, pretty = true)+"\n")
                       }
                       if (outputFormat.contains("triples")) {
                           //logger(0, result.graph.printTriples(detail = 1)+"\n")
                           logger(0, result1.graph.printTriples(
                                detail = 1,
                                extra = (node1, node2, relation) => {
                                    "\t"+decoder.features.ffDependencyPathv2(node1, node2, relation).toString.split("\n").filter(_.matches("^C1.*")).toList.toString+"\t"+decoder.features.localScore(node1, node2, relation).toString
                                })+"\n")
                       }
                       val amrdata2 = AMRTrainingData(training(i))
                       val result2 = oracle.decode(new Input(amrdata2, dependencies(i), oracle = true))
                       logger(0, "Oracle:")
                       if (outputFormat.contains("AMR")) {
                           val result3 = oracle.decode(new Input(amrdata2, dependencies(i), oracle = true, clearUnalignedNodes = false))
                           logger(0, result3.graph.root.prettyString(detail = 1, pretty = true)+"\n")
                       }
                       if (outputFormat.contains("triples")) {
                           //logger(0, result.graph.printTriples(detail = 1)+"\n")
                           logger(0, result2.graph.printTriples(
                                detail = 1,
                                extra = (node1, node2, relation) => {
                                    "\t"+oracle.features.ffDependencyPathv2(node1, node2, relation).toString.split("\n").filter(_.matches("^C1.*")).toList.toString+"\t"+decoder.features.localScore(node1, node2, relation).toString
                                })+"\n")
                       }
                       logger(0, "Dependencies:\n"+dependencies(i)+"\n")
                       logger(1, "Decoder features:\n"+result1.features+"\n")
                       logger(1, "Oracle features:\n"+result2.features+"\n")
                       result1.features -= result2.features
                       logger(1, "Gradient:\n"+result1.features+"\n")
                       result1.features },  // return gradient
                decoder.features.weights,
                training.size,
                passes,
                stepsize,
                false)

                //print(weights.unsorted)   // TODO: remove (not needed anymore because of shutdown hook)

            }

        } else {

            ///////////////// Decoding //////////////
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

            for ((block, i) <- Corpus.splitOnNewline(fromFile(options('amrOracleData)).getLines).filter(_.matches("(.|\n)*\n\\((.|\n)*")).zipWithIndex) {
                val line = input(i)
                logger(0, "Sentence:\n"+line+"\n")
                val tok = tokenized(i)
                val ner = nerFile(i)
                val stage1Result = stage1.decode(new Input(None,
                                                           tok.split(" "),
                                                           line.split(" "),
                                                           dependencies(i),
                                                           ner))
                logger(1, "Concepts:")
                for ((id, node) <- stage1Result.graph.getNodeById) {
                    logger(1, "id = "+id+" concept = "+node.concept)
                }
                logger(0, "Spans:")
                for ((span, i) <- stage1Result.graph.spans.zipWithIndex) {
                    logger(0, "Span "+(i+1).toString+":  "+span.words+" => "+span.amr)
                }
                logger(0, "")

                stage1Result.graph.normalizeInverseRelations
                stage1Result.graph.addVariableToSpans


                //val amrdata = AMRTrainingData(block)
                val amrdata2 = AMRTrainingData(block)   // 2nd copy for oracle
                logger(0, "Dependencies:\n"+dependencies(i)+"\n")
                logger(1, "Node.spans:")
                for (node <- amrdata2.graph.nodes) {
                    logger(1, node.concept+" "+node.spans.toList)
                }

                var decoderResult = stage1Result

                if (!options.contains('stage1Only)) {

                    // TODO: clean up this code

                val decoder = stage2.get
                decoderResult = decoder.decode(new Input(stage1Result.graph,
                                                         tok.split(" "),
                                                         dependencies(i)))

                if (options.contains('amrOracleData)) {
                val oracle = stage2Oracle.get
                val oracleResult = oracle.decode(new Input(amrdata2, dependencies(i), oracle = true))
                logger(0, "\nOracle Spans:")
                for ((span, i) <- amrdata2.graph.spans.zipWithIndex) {
                    logger(0, "Span "+(i+1).toString+":  "+span.words+" => "+span.amr)
                }
                logger(0, "")
                logger(0, "Oracle:\n"+oracleResult.graph.printTriples(detail = 1, extra = (node1, node2, relation) => {
                    "\t"+oracle.features.ffDependencyPathv2(node1, node2, relation).toString.split("\n").filter(_.matches("^C1.*")).toList.toString+"\t"+oracle.features.localScore(node1, node2, relation).toString
                    //"\n"+oracle.features.ffDependencyPathv2(node1, node2, relation).toString.split("\n").filter(_.matches("^C1.*")).toList.toString+"\nScore = "+decoder.features.localScore(node1, node2, relation).toString+"  Relevent weights:\n"+decoder.features.weights.slice(decoder.features.localFeatures(node1, node2, relation)).toString
                })+"\n")
                }//endif (options.contains('amrOracleData))

                logger(0, "AMR:\n"+decoderResult.graph.printTriples(detail = 1, extra = (node1, node2, relation) => {
                    "\t"+decoder.features.ffDependencyPathv2(node1, node2, relation).toString.split("\n").filter(_.matches("^C1.*")).toList.toString+"\t"+decoder.features.localScore(node1, node2, relation).toString
                    //"\n"+decoder.features.ffDependencyPathv2(node1, node2, relation).toString.split("\n").filter(_.matches("^C1.*")).toList.toString+"\nScore = "+decoder.features.localScore(node1, node2, relation).toString+"  Relevent weights:\n"+decoder.features.weights.slice(decoder.features.localFeatures(node1, node2, relation)).toString
                })+"\n")

                }//endif (!options.contains('stage1Only))

                if (outputFormat.contains("AMR")) {
                    println(decoderResult.graph.root.prettyString(detail=1, pretty=true) + '\n')
                }
                if (outputFormat.contains("triples")) {
                    println(decoderResult.graph.printTriples(detail = 1)+"\n")
                }
            }
        }
    }
}

