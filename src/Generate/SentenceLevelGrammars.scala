package edu.cmu.lti.nlp.amr.Generate
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.BasicFeatureVector._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}
import scala.io.Source
import java.io.BufferedWriter
import java.io.File
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import java.util.zip.GZIPOutputStream

object SentenceLevelGrammars {

    val usage = """Usage: scala -classpath . edu.cmu.lti.nlp.amr.Generate.SentenceLevelGrammars < amr_file > outfile"""
    type OptionMap = Map[Symbol, String]

    def parseOptions(map : OptionMap, list: List[String]) : OptionMap = {
        def isSwitch(s : String) = (s(0) == '-')
        list match {
            case Nil => map
            case "--features" :: value :: l =>           parseOptions(map + ('features -> value), l)
            case "--weights" :: value :: l =>            parseOptions(map + ('weights -> value), l)
            case "--dev" :: l =>                         parseOptions(map + ('dev -> "true"), l)
            case "--lemma-file" :: value :: l =>         parseOptions(map + ('lemmaFile -> value), l)
            case "--rule-inventory" :: value :: l =>     parseOptions(map + ('ruleInventory -> value), l)
            case "--no-basic-rules" :: l =>              parseOptions(map + ('noBasicRules -> "true"), l)
            case "--no-synthetic-rules" :: l =>          parseOptions(map + ('noSyntheticRules -> "true"), l)
            case "--no-abstract-rules" :: l =>           parseOptions(map + ('noAbstractRules -> "true"), l)
            case "--no-handwritten-rules" :: l =>        parseOptions(map + ('noHandWrittenRules -> "true"), l)
            case "--no-reference" :: l =>                parseOptions(map + ('noReference -> "true"), l)
            case "--kbest" :: value :: l =>              parseOptions(map + ('kbest -> value), l)
            case "--drop-sense-tags" :: l =>             parseOptions(map + ('dropSenseTags -> "true"), l)
            case "--predict-tree" :: l =>                parseOptions(map + ('predictTree -> "true"), l)
            case "--lowercase" :: l =>                   parseOptions(map + ('lowercase -> "true"), l)
            case "--output" :: value :: l =>             parseOptions(map + ('output -> value), l)
            case "-v" :: value :: tail =>                parseOptions(map ++ Map('verbosity -> value), tail)

            case option :: tail => println("Error: Unknown option "+option) 
                                   sys.exit(1)
        }
    }

    def main(args: Array[String]) {
        val options = parseOptions(Map(),args.toList)
        if (options.contains('help)) { println(usage); sys.exit(1) }

        if (options.contains('verbosity)) {
            verbosity = options('verbosity).toInt
        }

        val featureNames : Set[String] = Set() ++ options.getOrElse('features, "source,ruleGivenConcept,nonStopwordCount,nonStopwordCountPronouns").splitStr(",")

        val ruleInventory: RuleInventory = new RuleInventory(options.contains('dropSenseTags), options)
        ruleInventory.load(options('ruleInventory))

        val ruleModel = new SyntheticRules.Decoder(ruleInventory)
        ruleModel.weights.read(Source.fromFile(options('weights)).getLines)

        val kbest = options.getOrElse('kbest,"1").toInt
        lowercase = options.contains('lowercase)

        var i = 0
        for (block <- Corpus.getAMRBlocks(Source.stdin.getLines)) {
            logger(0,"**** Processing Block *****")
            logger(0,block)
            val data = if (!options.contains('noReference)) {
                    AMRTrainingData(block, lowercase)
                } else {
                    AMRTrainingData.fromAMR(block, lowercase)
                }

            //val pos =  projectPos(input(i).pos)
            val graph = data.toOracleGraph(clearUnalignedNodes = false)  // TODO: don't require aligned sentence (which data requires)
            if (options.contains('predictTree)) {
                logger(0, "-- Predicted tree --")
                graph.mkSpanningTree
                logger(0, graph.prettyString(detail=1, pretty=true))
            }
            // see http://stackoverflow.com/questions/10887828/string-to-gzipoutputstream
            var writer : BufferedWriter = null
            try {
                val gzFile = new GZIPOutputStream(new FileOutputStream(new File(options('output) + s"/grammar${i}.gz")))
                writer = new BufferedWriter(new OutputStreamWriter(gzFile, "UTF-8"))
                for (node <- graph.nodes) {
                    val corpusRules : List[(Rule, FeatureVector)] = if (!options.contains('noBasicRules)) {
                        ruleInventory.getRules(node)
                    } else { List() }
                    val passThroughRules : List[(Rule, FeatureVector)] = ruleInventory.passThroughRules(node, graph, kbest, ruleModel)
                    val syntheticRules : List[(Rule, FeatureVector)] = if (!options.contains('noSyntheticRules)) {
                        ruleModel.syntheticRules(SyntheticRules.Input(node, graph), kbest)
                    } else { List() }
                    logger(0, "concept = " + node.concept)
                    logger(0, "num children = " + node.children.size)
                    logger(0, "children = " + node.children.map(x => x._1).mkString(" "))
                    logger(0, "corpusRules.size = " + corpusRules.size)
                    //logger(0, "corpusRules = \n"+corpusRules.map(x => x._1.mkRule + " ||| " + x._2.toCdecFormat).mkString("\n"))
                    logger(0, "passThroughRules.size = " + passThroughRules.size)
                    //logger(0, "passThroughRules = \n"+passThroughRules.map(x => x._1.mkRule + " ||| " + x._2.toCdecFormat).mkString("\n"))
                    logger(0, "syntheticRules.size = " + syntheticRules.size)
                    //logger(0, "syntheticRules = \n"+syntheticRules.map(x => x._1.mkRule + " ||| " + x._2.toCdecFormat).mkString("\n"))
                    for (rule <- corpusRules ::: passThroughRules ::: syntheticRules) {
                        // Features
                        // corpusRules: rule given concept, realization given concept, and inverses
                        // passThroughRules: which kind of pass through, were inverse Morphy rules used
                        // syntheticRules: realization given concept, model score

                        //writer.append(rule.toString+" ||| synthetic=1\n")
                        //writer.append(rule._1.mkRule(withArgLabel=false)+" ||| synthetic=1\n")
                        rule._2 += FeatureVector(Map("ruleCount" -> 1.0))
                        writer.append(rule._1.mkRule(withArgLabel=false)+" ||| "+rule._2.toCdecFormat+'\n')
                    }
                }
                if (!options.contains('noHandWrittenRules)) {
                    writer.append("(X (X person) (ARG0_OF (X (X have-org-role-91) (ARG1 [X])))) ||| [1] ||| ruleCount=1.0 haveOrgRole=1.0\n") // Russia, Microsoft
                    writer.append("(X (X person) (ARG0_OF (X (X have-org-role-91) (ARG2 [X])))) ||| [1] ||| ruleCount=1.0 haveOrgRole=1.0\n") // officer, president
                    writer.append("(X (X person) (ARG0_OF (X (X have-org-role-91) (ARG1 [X]) (ARG2 [X])))) ||| [1] [2] ||| ruleCount=1.0 haveOrgRole=1.0\n")  // russian president, Microsoft CEO
                    writer.append("(X (X person) (ARG0_OF (X (X have-org-role-91) (ARG1 [X]) (ARG2 [X])))) ||| [2] of [1] ||| ruleCount=1.0 haveOrgRole=1.0\n")   // president of Russia, CEO of Microsoft
                    writer.append("(X (X person) (ARG0_OF (X (X have-org-role-91) (ARG1 [X]))) (NAME [X])) ||| [2] of [1] ||| ruleCount=1.0 haveOrgRole=1.0\n") // Bill Gates of Microsoft
                    writer.append("(X (X person) (ARG0_OF (X (X have-org-role-91) (ARG1 [X]))) (NAME [X])) ||| [2] from [1] ||| ruleCount=1.0 haveOrgRole=1.0\n") // Bill Gates from Microsoft
                    writer.append("(X (X person) (ARG0_OF (X (X have-org-role-91) (ARG2 [X]))) (NAME [X])) ||| [1] [2] ||| ruleCount=1.0 haveOrgRole=1.0\n")  // CEO Bill Gates, president Obama
                    writer.append("(X (X person) (ARG0_OF (X (X have-org-role-91) (ARG1 [X]) (ARG2 [X])) (NAME [X]))) ||| [1] [2] [3] ||| ruleCount=1.0 haveOrgRole=1.0\n")   // Microsoft CEO Bill Gates, U.S. president Barack Obama
                    writer.append("(X (X person) (ARG0_OF (X (X have-org-role-91) (ARG1 [X]) (ARG2 [X])) (NAME [X]))) ||| [2] of [1] [3] ||| ruleCount=1.0 haveOrgRole=1.0\n")    // CEO of Microsoft Bill Gates
                    //writer.append("(X (X person) (ARG0_OF (X (X have-org-role-91) (ARG1 [X]) (ARG2 [X])) (NAME [X]))) ||| [3] ( [2] of [1] ) ||| ruleCount=1.0 haveOrgRole=1.0\n")    // Bill Gates (CEO of Microsoft)
                }
            } finally {
                if (writer != null) {
                    writer.close
                }
            }
            //System.out.println("<seg grammar=\"" + options('output) + "/grammar" + i.toString + ".gz\"> " + Rule.graphToCFG(graph.root) + " </seg>")
            if (!options.contains('noReference)) {
                System.out.println("<seg id=\"" + i.toString + "\" grammar=\"" + options('output) + "/grammar" + i.toString + ".gz\"> " + Rule.graphToCFG(graph.root) + " ||| " + data.sentence.mkString(" ") + " </seg>")
            } else {
                System.out.println("<seg id=\"" + i.toString + "\" grammar=\"" + options('output) + "/grammar" + i.toString + ".gz\"> " + Rule.graphToCFG(graph.root) + " </seg>")
            }
            i += 1
        }
    }
}

