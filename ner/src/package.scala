package edu.cmu.lti.nlp

package object ner {
    type Sentence = Array[Token]     // A sentence is just a list of tokens
    implicit def doubleToMulAssoc(x: Double) = new MulAssoc(x)

}

