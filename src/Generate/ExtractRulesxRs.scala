package edu.cmu.lti.nlp.amr.Generate
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

object ExtractRulesxRs {

    val usage = """Usage: scala -classpath . edu.cmu.lti.nlp.amr.Generate.ExtractRulesxRs < amr_file > outfile"""
    type OptionMap = Map[Symbol, Any]

    def parseOptions(map : OptionMap, list: List[String]) : OptionMap = {
        def isSwitch(s : String) = (s(0) == '-')
        list match {
            case Nil => map
            case "-h" :: value :: tail =>
                      parseOptions(map ++ Map('help -> value.toInt), tail)
            case "-v" :: value :: tail =>
                      parseOptions(map ++ Map('verbosity -> value.toInt), tail)
            case option :: tail => println("Error: Unknown option "+option) 
                               sys.exit(1) 
      }
    }

    def main(args: Array[String]) {
        val options = parseOptions(Map(),args.toList)
        if (options.contains('help)) { println(usage); sys.exit(1) }

        if (options.contains('verbosity)) {
            verbosity = options('verbosity).asInstanceOf[Int]
        }

        for { block <- Corpus.splitOnNewline(Source.stdin.getLines)
              if (block matches "(.|\n)*\n\\((.|\n)*") } {
            logger(1,"**** Processsing Block *****")
            logger(1,block)
            logger(1,"****************************")
            val data = AMRTrainingData(block)
            val graph = data.toOracleGraph(clearUnalignedNodes = false)
            val sentence = data.sentence    // Tokenized sentence
            val spanArray : Array[List[Node]] = sentence.map(x => List())   // stores the projected spans on the sentence
            val spans : Map[String, (Option[Int], Option[Int])] = Map()     // stores the projected spans for each node
            computeSpans(graph, graph.root, spans, spanArray)
            extractRules(graph.root, sentence, spans, spanArray)
        }
    }

    def extractRules(node: Node, sent: Array[String], spans: Map[String, (Option[Int], Option[Int])], spanArray: Array[List[Node]]) {
        val (myStart, myEnd) = spans(node.id)
        case class Child(label: String, node: Node, start: Int, end: Int)
        val children : List[Child] = node.children.filter(x => spans(x._2.id)._1 != None).map(x => {val (start, end) = spans(x._2.id); Child(x._1, x._2, start.get, end.get)}).sortBy(x => x.start)
        if (myStart != None && !(0 until children.size-1).exists(i => children(i).start > children(i+1).end)) { // check for no overlapping child spans (if so, no rule can be extracted)
            var outsideLower = myStart.get
            do { outsideLower -= 1 } while (outsideLower >= 0 && spanArray(outsideLower).size == 0)
            outsideLower += 1
            var outsideUpper = myEnd.get
            do { outsideUpper += 1 } while (outsideUpper < sent.size && spanArray(outsideUpper).size == 0)
            outsideUpper -= 1

            // We will extract the largest rule (with the most lexical items)
            // and delete spans of lexical items to get the other rules
            val prefix : List[String] = sent.slice(outsideLower, myStart.get).toList
            /*for (i <- myStart.get until myEnd.get) {
                spanArray(i) = spanArray(i).filter(_.id != node.id)
            }
            for (i <- node.spans(0).start until node.spans(0).end) {    // spans.size > 0 because myStart != None
                spanArray(i) = node :: spanArray(i)
            }*/
            var rest : Array[(String, List[String])] = (0 until children.size-1).map(
                i => (children(i).label, sent.slice(children(i).end, children(i+1).start).toList)).toArray
            val end : (String, List[String]) = if (children.size > 0) {
                (children(children.size-1).label, sent.slice(children(children.size-1).end, outsideUpper).toList)
            } else {
                ("", List())
            }

            // The rule is prefix.mkString(" ")+" "+rest.map(x => x._1+" "+x._2.mkString(" ")).mkString(" ")
            val concept = node.concept.replaceAll("""\(""", "-LBR-").replaceAll("""\)""", "-RBR-")
            val labels = children.sortBy(_.label).map(x => "["+x.label.drop(1).toUpperCase.replaceAll("-","")+"]").mkString(" ")
            val ruleARGS = (prefix ::: (rest.toList ::: List(end)).map(x => "["+x._1.drop(1).toUpperCase.replaceAll("-","")+"] "+x._2.mkString(" ")).toList).mkString(" ")
            val rule = (prefix ::: (rest.toList ::: List(end)).zipWithIndex.sortBy(_._1._1).zipWithIndex.sortBy(_._1._2).map(x => "["+(x._2+1).toString+"] "+x._1._1._2.mkString(" ")).toList).mkString(" ")
            println("(X (X "+concept+") "+labels+") ||| "+ruleARGS+" ||| "+rule)
        }

        /*def extracRecursive(prefix: List[String], children: List[Child]) {
            
        }*/
    }

    def computeSpans(graph: Graph, node: Node, spans: Map[String, (Option[Int], Option[Int])], spanArray: Array[List[Node]]) : (Option[Int], Option[Int]) = {
        var myStart : Option[Int] = None
        var myEnd : Option[Int] = None
        if (node.spans.size > 0) {
            myStart = Some(graph.spans(node.spans(0)).start)
            myEnd = Some(graph.spans(node.spans(0)).end)
        }
        for ((_, child) <- node.topologicalOrdering) {
            val (start, end) = computeSpans(graph, child, spans, spanArray)
            if (myStart != None && myEnd != None) {
                if (start != None && end != None) {
                    myStart = Some(min(myStart.get, start.get))
                    myEnd = Some(max(myEnd.get, end.get))
                }
            } else {
                myStart = start
                myEnd = end
            }
        }
        if (myStart != None && myEnd != None) {
            for (i <- myStart.get until myEnd.get) {
                spanArray(i) = node :: spanArray(i)
            }
        }
        spans(node.id) = (myStart, myEnd)
        return (myStart, myEnd)
    }
}

