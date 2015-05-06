package edu.cmu.lti.nlp.amr.Generate
import edu.cmu.lti.nlp.amr._

import scala.collection.mutable.ArrayBuffer
import scala.collection.{mutable => m, immutable => i}  // m.Set, m.Map, i.Set, i.Map

object Decoder {
    def hypergraph(amr: Node, ruleInventory: RuleInventory) : Hypergraph[String, Rule]

    def hypergraph(amr: Node, state: List[Int], builder: Hypergraph.Builder, visited: m.Set[List[Int]]) : Boolean   // no state
    def hypergraph(amr: Node, state: List[Int], builder: Hypergraph.Builder, visited: m.Map[List[Int], List[String]]) : List[String]    // with state
        // recursive function
        // calls .match for every rule that matches the concept, and 
        // recursively calls hypergraph on the the unmatched children if that node hasn't been visited, otherwise it uses the cached states
        // returns a list of the states that can be created (remember the hypergraph node is (List[Int], String)
        // or empty list if no states can be created
        // if the rule matches, and the states match, or state mismatch is allowed, add an edge with the rule to the builder
}

