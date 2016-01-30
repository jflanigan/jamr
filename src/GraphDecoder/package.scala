package edu.cmu.lti.nlp.amr
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.Train._
import edu.cmu.lti.nlp.amr.FastFeatureVector._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

package object GraphDecoder {
    type OptionMap = Map[Symbol, String]

    def getFeatures(options: OptionMap) : List[String] = {
        options.getOrElse('stage2Features, "conceptBigram,rootConcept").split(",").toList.filter(x => x != "edgeId" && x != "labelWithId")
    }

    def loadLabelset(filename: String) : Array[(String, Int)] = {
        Source.fromFile(filename).getLines().toArray.map(x => {
            val split = x.split(" +")
            (split(0), if (split.size > 1) { split(1).toInt } else { 1000 })
        })
    }

    def getLabelset(options: OptionMap) : Array[(String, Int)] = {
        return loadLabelset(options('stage2Labelset))   // TODO: check for errors
    }

    def Decoder(options: OptionMap) : GraphDecoder.Decoder = {
        if (!options.contains('stage2Labelset)) {
            System.err.println("Error: No labelset file specified"); sys.exit(1)
        }

        val labelset: Array[(String, Int)] = getLabelset(options)

        val features = getFeatures(options)
        logger(0, "features = " + features)

        val connected = !options.contains('stage2NotConnected)
        logger(0, "connected = " + connected)

        if (!options.contains('stage2Decoder)) {
            System.err.println("Error: No stage2 decoder specified"); sys.exit(1)
        }

        val decoder: Decoder = options('stage2Decoder) match {
            case "Alg1" => new Alg1(options, features, labelset)
            case "Alg1a" => new Alg1(options, features, labelset, connectedConstraint = "and")
            case "Alg2" => new Alg2(options, features, labelset, connected)
            case "Greedy" => new Greedy(options, features, labelset)
            //case "DD" => new DualDecomposition(features, labelset, 1)
            case "LR" => new LagrangianRelaxation(options, features, labelset)
            case x => { System.err.println("Error: unknown stage2 decoder " + x); sys.exit(1) }
        }

        val outputFormat = options.getOrElse('outputFormat,"triples").split(",").toList
        if (outputFormat.contains("AMR") && !connected) {
            println("Cannot have both -stage2NotConnected flag and --outputFormat \"AMR\""); sys.exit(1)
        }

        if (options('stage2Decoder) == "Alg1" && outputFormat.contains("AMR")) {
            System.err.println("Cannot have --outputFormat \"AMR\" for stage2 Alg1 (graph may not be connected!)")
            sys.exit(1)
        }

        return decoder
    }

    def Oracle(options: OptionMap) : GraphDecoder.Decoder = {
        return new Oracle(options, getFeatures(options), getLabelset(options).map(x => x._1))
    }

    def CostDiminished(options: OptionMap) : GraphDecoder.Decoder = {
        val decoder = Decoder(options)
        return new CostAugmented(decoder, -100000000000.0, 0.5, options)
    }
}

