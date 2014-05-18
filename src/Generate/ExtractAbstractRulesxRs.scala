package edu.cmu.lti.nlp.amr.Generate
import edu.cmu.lti.nlp.amr._

import scala.util.matching.Regex
import scala.collection.mutable.{Map, Set, ArrayBuffer}

object ExtractAbstractRulesxRs {

    val usage = """Usage: scala -classpath . edu.cmu.lti.nlp.amr.Generate.ExtractAbstractRulesxRs < amr_file > outfile"""
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
            logger(0,"**** Processsing Block *****")
            logger(0,block)
            val data = AMRTrainingData(block)
            val graph = data.toOracleGraph(clearUnalignedNodes = false)
            val sentence = data.sentence    // Tokenized sentence
            val spans : Map[String, (Option[Int], Option[Int])] = Map()     // stores the projected spans for each node
            val spanArray : Array[Boolean] = sentence.map(x => false)    // stores the endpoints of the spans
            computeSpans(graph, graph.root, spans, spanArray)
            //logger(0,"spanArray = "+spanArray.zip(sentence).toList.toString)
            logger(0,"****** Extracted rules ******")
            extractRules(graph, graph.root, sentence, spans, spanArray)
            logger(0,"")
        }
    }

    def extractRules(graph: Graph, node: Node, sent: Array[String], spans: Map[String, (Option[Int], Option[Int])], spanArray: Array[Boolean]) {
        val (myStart, myEnd) = spans(node.id)
        case class Child(label: String, node: Node, start: Int, end: Int)
        val children : List[Child] = node.children.filter(x => spans(x._2.id)._1 != None).map(x => {val (start, end) = spans(x._2.id); Child(x._1, x._2, start.get, end.get)}).sortBy(x => x.end)
        //logger(1, "children = "+children.toString)
        if (myStart != None && children.size > 0 && !(0 until children.size-1).exists(i => children(i).start > children(i+1).end)) { // check for no overlapping child spans (if so, no rule can be extracted)
            var outsideLower = myStart.get
            //do { outsideLower -= 1 } while (outsideLower >= 0 && !spanArray(outsideLower))
            //outsideLower += 1
            var outsideUpper = myEnd.get
            //while (outsideUpper < sent.size && !spanArray(outsideUpper)) {
            //    outsideUpper += 1
            //}

            // We will extract the largest rule (with the most lexical items)
            // and delete spans of lexical items to get the other rules
            val (mySpan_start, mySpan_end) = if (node.spans.size > 0) {
                val mySpan = graph.spans(node.spans(0))
                (mySpan.start, mySpan.end)
            } else {
                (0,0)
            }
            val save : Array[String] = sent.slice(mySpan_start, mySpan_end)
            for (i <- Range(mySpan_start, mySpan_end)) {
                sent(i) = "[CONCEPT]"
            }
            val prefix : List[String] = if (children.size > 0) {
                sent.slice(outsideLower, children.map(x => x.start).min).toList
            } else {
                sent.slice(outsideLower, outsideUpper).toList
            }
            var rest : Array[(String, List[String])] = (0 until children.size-1).map(
                i => (children(i).label, sent.slice(children(i).end, children(i+1).start).toList)).toArray
            val end : (String, List[String]) = if (children.size > 0) {
                (children(children.size-1).label, sent.slice(children(children.size-1).end, outsideUpper).toList)
            } else {
                ("", List())
            }
            //logger(1, "prefix = "+prefix.toString)
            //logger(1, "rest = "+rest.toList.toString)
            //logger(1, "end = "+end.toString)

            // The rule is prefix.mkString(" ")+" "+rest.map(x => x._1+" "+x._2.mkString(" ")).mkString(" ")
            val concept = node.concept.replaceAll("""\(""", "-LBR-").replaceAll("""\)""", "-RBR-")
            val labels = children.sortBy(_.label).map(x => "["+x.label.drop(1).toUpperCase.replaceAll("-","")+"]").mkString(" ")
            val ruleARGS = (prefix ::: (rest.toList ::: List(end)).map(x => "["+x._1.drop(1).toUpperCase.replaceAll("-","")+"] "+x._2.mkString(" ")).toList).mkString(" ")
            val rule = (prefix ::: (rest.toList ::: List(end)).zipWithIndex.sortBy(_._1._1).zipWithIndex.sortBy(_._1._2).map(x => "["+(x._2+1).toString+"] "+x._1._1._2.mkString(" ")).toList).mkString(" ")
            //logger(0, "(X (X "+concept+") "+labels+") ||| "+ruleARGS+" ||| "+rule)
            logger(0, "(X (X "+concept+") "+labels+") ||| "+ruleARGS)
            println("(X (X "+concept+") "+labels+") ||| "+rule)
            for (i <- Range(0, save.size)) {
                sent(i + mySpan_start) = save(i)
            }
        }

        for (child <- children) {
            extractRules(graph, child.node, sent, spans, spanArray)
        }
    }

    def computeSpans(graph: Graph, node: Node, spans: Map[String, (Option[Int], Option[Int])], spanArray: Array[Boolean]) : (Option[Int], Option[Int]) = {
        var myStart : Option[Int] = None
        var myEnd : Option[Int] = None
        if (node.spans.size > 0) {
            myStart = Some(graph.spans(node.spans(0)).start)
            myEnd = Some(graph.spans(node.spans(0)).end)
            spanArray(myStart.get) = true
            spanArray(myEnd.get - 1) = true
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
        spans(node.id) = (myStart, myEnd)
        return (myStart, myEnd)
    }
}

