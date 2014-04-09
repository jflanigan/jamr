package edu.cmu.lti.nlp.amr.Train

abstract class AbstractFeatureVector(labelset : Array[String]) {
    def += (v: AbstractFeatureVector) : Unit
    def -= (v: AbstractFeatureVector) : Unit
    def unsorted : String
    class SSGD
}

