package edu.cmu.lti.nlp.amr.Generate
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

object ExtractRules {

    val usage = """Usage: scala -classpath . edu.cmu.lti.nlp.amr.Generate.ExtractRules < amr_file > outfile"""
    type OptionMap = Map[Symbol, String]

    def parseOptions(map : OptionMap, list: List[String]) : OptionMap = {
        def isSwitch(s : String) = (s(0) == '-')
        list match {
            case Nil => map
            case "--output" :: value :: l =>             parseOptions(map + ('output -> value), l)
            case "--drop-sense-tags" :: l =>             parseOptions(map + ('dropSenseTags -> "true"), l)
            case "--lowercase" :: l =>                   parseOptions(map + ('lowercase -> "true"), l)
            case "--training-loss" :: value :: l =>      parseOptions(map + ('trainingLoss -> value), l)
            case "--training-initial-weights"::value::l => parseOptions(map + ('trainingInitialWeights -> value), l)
            case "--training-cost-scale" :: value ::l => parseOptions(map + ('trainingCostScale -> value), l)
            case "--training-prec-recall" :: value::l => parseOptions(map + ('trainingPrecRecallTradeoff -> value), l)
            case "--training-l2-strength" :: value::l => parseOptions(map + ('trainingL2RegularizerStrength -> value), l)
            case "--training-optimizer" :: value :: l => parseOptions(map + ('trainingOptimizer -> value), l)
            case "--training-output" :: value :: l =>    parseOptions(map + ('trainingOutputFile -> value), l)
            case "--training-stepsize" :: value :: l =>  parseOptions(map + ('trainingStepsize -> value), l)
            case "--training-passes" :: value :: l =>    parseOptions(map + ('trainingPasses -> value), l)
            case "--training-avg-weights" :: l =>        parseOptions(map + ('trainingAvgWeights -> "true"), l)
            case "--training-save-interval"::value::l => parseOptions(map + ('trainingSaveInterval -> value), l)
            case "--training-data" :: value :: tail =>   parseOptions(map + ('trainingData -> value), tail)
            case "--training-dev" :: value :: tail =>    parseOptions(map + ('trainingDev -> value), tail)
            //case "--ignore-parser-errors" :: l =>        parseOptions(map + ('ignoreParserErrors -> "true"), l)
            case "--dependencies" :: value :: tail =>    parseOptions(map + ('dependencies -> value), tail)
            case "--ner" :: value :: tail =>             parseOptions(map + ('ner -> value), tail)
            case "--snt" :: value :: tail =>             parseOptions(map ++ Map('notTokenized -> value), tail)
            case "--tok" :: value :: tail =>             parseOptions(map ++ Map('tokenized -> value), tail)
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

        val input : Array[Input] = Input.loadInputfiles(options)
        val pos = input.map(x => x.pos)

        val ruleInventory: RuleInventory = new RuleInventory(dropSenses = options.contains('dropSenseTags))
        ruleInventory.extractFromCorpus(io.Source.stdin.getLines, pos, options.contains('lowercase))

        ruleInventory.save(options('output))
    }
}

