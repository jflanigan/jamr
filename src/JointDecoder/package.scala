package edu.cmu.lti.nlp.amr
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

package object JointDecoder {

    def Decoder(options: Map[Symbol, String], oracle: Boolean = false) : JointDecoder.Decoder = {

        val stage1Features = options.getOrElse('stage1Features,"length,count").split(",").toList
        if (!options.contains('stage1ConceptTable)) {
            logger(0,"Error: No concept table specified"); sys.exit(1)
        }
        val conceptTable = Source.fromFile(options('stage1ConceptTable)).getLines.map(x => new ConceptInvoke.PhraseConceptPair(x)).toArray

        if (!options.contains('stage2Labelset)) {
            logger(0,"Error: No labelset file specified"); sys.exit(1)
        }

        val stage2Labelset: Array[(String, Int)] = GraphDecoder.getLabelset(options)
        val stage2Features = GraphDecoder.getFeatures(options)

        val decoder: Decoder = if (oracle) {
            new Oracle(stage1Features, conceptTable, stage2Features, stage2Labelset)
        } else {
            options('jointDecoder) match {
//                case "ILP" => new ILP(stage1Features, conceptTable, stage2Features, stage2Labelset)
                case x => { logger(0,"Error: unknown joint decoder " + x); sys.exit(1) }
            }
        }

        return decoder
    }
}

