package edu.cmu.lti.nlp.amr
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

package object Generate {
    def labelStr(label: String) : String = {
        return label.drop(1).toUpperCase.replaceAll("-","_")
    }
}

