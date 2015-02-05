package edu.cmu.lti.nlp.amr.Generate.SyntheticRules
import edu.cmu.lti.nlp.amr._
import edu.cmu.lti.nlp.amr.Generate._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

case class Input(node: Node, graph: Graph)

