package edu.cmu.lti.nlp.amr
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

package object JointDecoder {

    def Decoder(options: Map[Symbol, String], oracle: Boolean = false) : JointDecoder.Decoder = {

        val stage1Features = options.getOrElse('stage1Features,"length,count").split(",").toList
        if (!options.contains('stage1ConceptTable)) {
            System.err.println("Error: No concept table specified"); sys.exit(1)
        }
        val conceptTable = Source.fromFile(options('stage1ConceptTable)).getLines.map(x => ConceptInvoke.PhraseConceptPair(x)).toArray

        if (stage1Features.contains("phrase") && !options.contains('stage1PhraseCounts)) {
            System.err.println("Error: phrase features specified but no phrase counts specified"); sys.exit(1)
        }
        val phraseCounts = Source.fromFile(options('stage1PhraseCounts)).getLines.map(x => (x.split(" ").init.toList, x.split(" ").last.toInt)).toMap

        if (!options.contains('stage2Labelset)) {
            System.err.println("Error: No labelset file specified"); sys.exit(1)
        }

        val stage2Labelset: Array[(String, Int)] = GraphDecoder.getLabelset(options)
        val stage2Features = GraphDecoder.getFeatures(options)

        val decoder: Decoder = if (oracle) {
            new Oracle(options, stage1Features, conceptTable, phraseCounts, stage2Features, stage2Labelset)
        } else {
            options('jointDecoder) match {
//                case "ILP" => new ILP(stage1Features, conceptTable, stage2Features, stage2Labelset)
                case x => { System.err.println("Error: unknown joint decoder " + x); sys.exit(1) }
            }
        }

        return decoder
    }
}

