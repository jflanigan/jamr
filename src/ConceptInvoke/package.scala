package edu.cmu.lti.nlp.amr
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.Train._
import edu.cmu.lti.nlp.amr.BasicFeatureVector._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

package object ConceptInvoke {
    type OptionMap = Map[Symbol, String]

    def Decoder(options: OptionMap, oracle: Boolean = false) : Decoder = {
        val stage1Features = options.getOrElse('stage1Features,"length,count").split(",").toList
        logger(0, "Stage1 features = " + stage1Features)

        if (!options.contains('stage1ConceptTable)) {
            System.err.println("Error: No concept table specified"); sys.exit(1)
        }
        val conceptFile = options('stage1ConceptTable)
        val conceptTable = Source.fromFile(conceptFile).getLines.map(x => PhraseConceptPair(x)).toArray

        if (stage1Features.contains("phrase") && !options.contains('stage1PhraseCounts)) {
            System.err.println("Error: phrase features specified but no phrase counts specified"); sys.exit(1)
        }
        val phraseCounts = Source.fromFile(options('stage1PhraseCounts)).getLines.map(x => (x.split(" ").init.toList, x.split(" ").last.toInt)).toMap

        if (oracle) {   // TODO: what about cost augmented as oracle?
            new Oracle(options, stage1Features, conceptTable, phraseCounts)
        } else {
            new Decoder1(options, stage1Features, conceptTable, phraseCounts)
        }
    }
}

