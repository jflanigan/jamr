package edu.cmu.lti.nlp.amr

import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

// Case class for BIO tagged entities

case class Entity(start: Int, end: Int, label: String)

object Entity {

    def entitiesFromConll(conllStr: String, column: Int = 1) : Array[Entity] = {
        // Returns the entities from a conll BIO tagging string (tokens separated by "\n", fields by "\t")
        val conll = conllStr.split("\n").map(x => x.split("\t"))    // WARNING: conllStr should not end with a '\n' (otherwise our test for conll(i)(1) != "O" might be index out of bounds because the line is empty)
        var i = 0
        val entities : ArrayBuffer[Entity] = ArrayBuffer()
        while (i < conll.size) {
            if (conll(i)(column) != "O") {
                assert(conll(i)(column).matches("(B|I|L|U)-.*"), "Data is not in conll BIO tagging format")  // UIUC format
                val label = conll(i)(column).drop(2)
                val start = i
                i += 1
                while (i < conll.size && (conll(i)(column) == "I-"+label || conll(i)(column) == "L-"+label)) {
                    i +=1
                }
                val end = i
                entities += Entity(start, end, label)
            } else {
                i += 1
            }
        }
        return entities.toArray
    }

}

