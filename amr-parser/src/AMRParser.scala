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
import scala.util.matching.Regex
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator._

import edu.cmu.lti.nlp.amr.GraphDecoder._

/****************************** Driver Program *****************************/
object AMRParser {

    val usage = """Usage:
scala -classpath . edu.cmu.lti.nlp.amr.AMRParser -l labelset --train < trainfile > output_weights
scala -classpath . edu.cmu.lti.nlp.amr.AMRParser -w weights -l labelset < input > output"""
//TODO: tagset option
    type OptionMap = Map[Symbol, Any]

    def parseOptions(map : OptionMap, list: List[String]) : OptionMap = {
        def isSwitch(s : String) = (s(0) == '-')
        list match {
            case Nil => map
            case "--train" :: tail =>
                      parseOptions(map ++ Map('train -> true), tail)
            case "-w" :: value :: tail =>
                      parseOptions(map ++ Map('weights -> value), tail)
            case "-l" :: value :: tail =>
                      parseOptions(map ++ Map('labelset -> value), tail)
            case "--decoder" :: value :: tail =>
                      parseOptions(map ++ Map('decoder -> value), tail)
            case "--features" :: value :: tail =>
                      parseOptions(map ++ Map('features -> value), tail)
            case "--outputFormat" :: value :: tail =>
                      parseOptions(map ++ Map('outputFormat -> value), tail)
            case "--dependencies" :: value :: tail =>
                      parseOptions(map ++ Map('dependencies -> value), tail)
            case "-nc" :: tail =>
                      parseOptions(map ++ Map('notConnected -> true), tail)
            case "-v" :: value :: tail =>
                      parseOptions(map ++ Map('verbosity -> value.toInt), tail)
            case "-p" :: value :: tail =>
                      parseOptions(map ++ Map('passes -> value.toInt), tail)
            case string :: opt2 :: tail if isSwitch(opt2) => 
                      parseOptions(map ++ Map('infile -> string), list.tail)
            case string :: Nil =>  parseOptions(map ++ Map('infile -> string), list.tail)
            case option :: tail => println("Error: Unknown option "+option) 
                               sys.exit(1) 
      }
    }

    def main(args: Array[String]) {

        if (args.length == 0) { println(usage); sys.exit(1) }

        val options = parseOptions(Map(),args.toList)
        if (options.contains('verbosity)) {
            verbosity = options('verbosity).asInstanceOf[Int]
        }

        if (!options.contains('labelset)) {
            System.err.println("Error: No labelset file specified")
            sys.exit(1)
        }
        val labelset: Array[(String, Int)] = Source.fromFile(options('labelset).asInstanceOf[String]).getLines().toArray.map(x => {
            val split = x.split(" +")
            (split(0), if (split.size > 1) { split(1).toInt } else { 100 })
        })
        //(x.split(" +")(0), x.split(" +").zipWithIndex.map(x => (x._2, x._2)).toMap.getOrElse(1,"100").toInt))

        var features = List("conceptBigram", "rootConcept")
        if (options.contains('features)) {
            features = options('features).asInstanceOf[String].split(",").toList
        }
        logger(0, "features = " + features)

        val connected = !options.contains('notConnected)
        logger(0, "connected = " + connected)

        var outputFormat = List("triples")
        if (options.contains('outputFormat)) {
            outputFormat = options('outputFormat).asInstanceOf[String].split(",").toList
        }

        if (outputFormat.contains("AMR") && !connected) {
            println("Cannot have both -nc flag and --outputFormat \"AMR\"")
            sys.exit(1)
        }

        if (!options.contains('decoder)) {
            System.err.println("Error: No decoder specified")
            sys.exit(1)
        }
        val decoder: Decoder = options('decoder).asInstanceOf[String] match {
            case "Alg1" => new Alg1(features, labelset)
            case "Alg2" => new Alg2(features, labelset, connected)
            case "DD" => new DualDecomposition(features, labelset, 1)
            case x => { System.err.println("Error: unknown decoder " + x); sys.exit(1) }
        }
        if (options('decoder).asInstanceOf[String] == "Alg1" && outputFormat.contains("AMR")) {
            println("Cannot have --outputFormat \"AMR\" for Alg1 (graph may not be connected!)")
            sys.exit(1)
        }

        val oracle = new GraphDecoder.Oracle(features)

        if (options contains 'train) {

            ////////////////// Training ////////////////

            var passes = 20
            if (options.contains('passes)) { passes = options('passes).asInstanceOf[Int] }

            System.err.print("Loading training data...")
            val training: Array[String] = (for {
                block <- Corpus.splitOnNewline(io.Source.stdin.getLines())
                if block.matches("(.|\n)*\n\\((.|\n)*")     // needs to contain some AMR
            } yield block).toArray
            val dependencies = if (options.contains('dependencies)) {
                (for {
                    block <- Corpus.splitOnNewline(Source.fromFile(options('dependencies).asInstanceOf[String]).getLines())
                    if block.matches("(.|\n)*\n\\((.|\n)*")     // needs to contain some AMR
                } yield block).toArray
            } else {
                training.map(x => "")
            }
            System.err.println(" done")

            val weights = Perceptron.learnParameters(
                //i => decoder.decode(Corpus.toAMRTriple(training(i)).toInput).features,
                i => { val amrdata = Corpus.toAMRTriple(training(i))
                       val result = decoder.decode(Input(amrdata.toInputGraph,
                                                         amrdata.sentence,
                                                         dependencies(i).split("\n").map(x => Dependency.fromStanford(x)),
                                                         Array()))
                       logger(0, "AMR: ")
                       if (outputFormat.contains("AMR")) {
                           logger(0, result.graph.root.prettyString(detail = 1, pretty = true)+"\n")
                       }
                       if (outputFormat.contains("triples")) {
                           logger(0, result.graph.printTriples(detail = 1)+"\n")
                       }
                       result.features },
                //i => oracle.decode(Corpus.toAMRTriple(training(i)).toOracle).features,
                i => { val amrdata = Corpus.toAMRTriple(training(i))
                       val result = oracle.decode(Input(amrdata.toOracleGraph(clearUnalignedNodes = true),
                                                        amrdata.sentence,
                                                        dependencies(i).split("\n").map(x => Dependency.fromStanford(x)),
                                                        Array()))
                       logger(0, "Oracle: ")
                       if (outputFormat.contains("AMR")) {
                           val result2 = oracle.decode(
                                            Input(amrdata.toOracleGraph(clearUnalignedNodes = false),
                                                  amrdata.sentence,
                                                  dependencies(i).split("\n").map(x => Dependency.fromStanford(x)),
                                                  Array()))
                           logger(0, result2.graph.root.prettyString(detail = 1, pretty = true)+"\n")
                       }
                       if (outputFormat.contains("triples")) {
                           logger(0, result.graph.printTriples(detail = 1)+"\n")
                       }
                       result.features },
                decoder.features.weights,
                training.size,
                passes,
                false)

            print(weights.unsorted)

        } else {

            ///////////////// Decoding //////////////

            if (!options.contains('weights)) {
                System.err.println("Error: No weights file specified")
                sys.exit(1)
            }
            val weightfile : String = options('weights).asInstanceOf[String]

            logger(0, "Reading weights")
            decoder.features.weights.read(Source.fromFile(weightfile).getLines())
            logger(0, "done")

            val dependencies: Array[String] = if (options.contains('dependencies)) {
                (for {
                    block <- Corpus.splitOnNewline(Source.fromFile(options('dependencies).asInstanceOf[String]).getLines())
                    if block.matches("(.|\n)*\n\\((.|\n)*")     // needs to contain some AMR
                } yield block).toArray
            } else {
                new Array(0)
            }

            for ((block, i) <- Corpus.splitOnNewline(io.Source.stdin.getLines()).filter(_.matches("(.|\n)*\n\\((.|\n)*")).zipWithIndex) {
                val amrdata = Corpus.toAMRTriple(block)
                val decoderResult = decoder.decode(Input(amrdata.toInputGraph,
                                                         amrdata.sentence,
                                                         ArrayToMyArray(dependencies).getOrElse(i,"").split("\n").map(x => Dependency.fromStanford(x)),
                                                         Array()))
                if (outputFormat.contains("AMR")) {
                    println(decoderResult.graph.root.prettyString(detail=1, pretty=true) + '\n')
                }
                if (outputFormat.contains("triples")) {
                    logger(0, decoderResult.graph.printTriples(detail = 1)+"\n")
                }
            }
        }
    }
}

