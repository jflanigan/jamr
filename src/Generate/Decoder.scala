package edu.cmu.lti.nlp.amr.Generate
import edu.cmu.lti.nlp.amr._

import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable => m, immutable => i}  // m.Set, m.Map, i.Set, i.Map

object Decoder {
    def hypergraph(amr: Node, ruleInventory: RuleInventory) : Hypergraph[String, Rule]
}

