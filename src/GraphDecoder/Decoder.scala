package edu.cmu.lti.nlp.amr.GraphDecoder
import edu.cmu.lti.nlp.amr._

import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

abstract class Decoder {
    val features : Features   // maybe this should be renamed ff?

    def decode(i: Input) : DecoderResult
}

