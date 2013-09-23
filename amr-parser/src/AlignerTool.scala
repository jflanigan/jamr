package edu.cmu.lti.nlp.amr

import java.awt.FlowLayout
import java.awt.Color
import java.awt.Font
import javax.swing.JFrame
import javax.swing.JList
import javax.swing.JOptionPane
import javax.swing.JScrollPane
import javax.swing.event.ListSelectionListener
import javax.swing.event.ListSelectionEvent
import javax.swing.ListSelectionModel

class AlignerToolFrame(corpus: LazyArray[AMRTriple]) extends JFrame("AMR Aligner Tool v0.1a") {
    val colorNames = Array[Object]("Black", "Blue", "Cyan", "Dark Gray", "Gray", "Green", "Light Gray", "Magenta", "Orange", "Pink", "Red", "White", "Yellow")
    val colors = Array(Color.BLACK, Color.BLUE, Color.CYAN, Color.DARK_GRAY, Color.GRAY, Color.GREEN, Color.LIGHT_GRAY, Color.MAGENTA, Color.ORANGE, Color.PINK, Color.RED, Color.WHITE, Color.YELLOW)

    setLayout(new FlowLayout())
    val wordJList = new JList(colorNames)
    val amrJList = new JList(colorNames)
    val jLists = Array(wordJList, amrJList)
    var update = true
    for ((l,i) <- jLists.zipWithIndex) {
        l.setFont(new Font(Font.MONOSPACED, Font.BOLD, 12))
        l.setVisibleRowCount(10)
        l.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)
        add(new JScrollPane(l))

        l.addListSelectionListener(
            new ListSelectionListener {
                def valueChanged(event : ListSelectionEvent) {
                    if (update) {
                        update = false
                        jLists((i+1)%2).setSelectedIndices(l.getSelectedIndices)
                    }
                    /*val selection : Array[Int] = l.getSelectedIndices
                    if (newSelection(i) != selection) {
                        newSelection(i) = selection
                        newSelection((i+1)%2) = selection
                        jLists((i+1)%2).setSelectedIndices(selection)
                    } */
                    /*if (update(i) > 0) {
                        println("Ignoring update")
                        //update(i) -= 1
                    } else {
                        update((i+1)%2) = 1
                        jLists((i+1)%2).setSelectedIndices(l.getSelectedIndices)
                    }*/
                    //getContentPane().setBackground(colors(colorJList.getSelectedIndex()))
                }
            }
        )
    }
}

case class AMRTriple(sentence: Array[String], graph: Graph, spans: Array[Spans], annotators: Array[String])

def splitOnNewline(iterator: Iterator[String]) : Iterator[String] = {   // This treats more than one newline in a row as a single newline
    for {
        x <- iterator if x != '\n'
        p = (x :: it.takewhile(_ != "\n").toList).mkString
    } yield p
}

def getUlfString(string: String) : Map[String,String] = {
    // returns a map representation of Ulf's weird string representation
    val split = string.replaceAll("""#|\n"""," ").split(" ::")
    val map = Map[String,String]()
    for (x <- split) {
        line = x.split(" ")
        map += (("::"+line(0)) -> line.tail.mkString(" "))
    }
    return map
}

def toAMRTriple(input: String) : AMRTriple = {
    val lines = input.split("\n")
    val amrstr = lines.filterNot(_.matches("^#.*")).mkstring(" ")
    val tokenized = lines.filter(_.matches("^# ::tok .*"))
    assert(tokenized.size == 1, "Incorrect number of tokenized ::tok ")
    val spanlines = lines.filter(_.matches("^# ::alignment .*"))
    assert(spanlines.size > 0, "Missing alignments")
    
    val graph = Graph.parse(amrstr)
    //val TokExtractor = "^# ::tok (.*)".r
    //val ("^# ::tok (.*)".r)(sentence) = tokenized(0)
    //val SpanExtractor = "^ ::alignment ([^:])+ ::annotator ([^:])".r

    val sentence = getUlfString(tokenized(0))("::tok")
    var spans = List[ArrayBuffer[Span]]()
    for (spanline <- spanlines) {
        val ulfstr = getUlfString(spanline)
        val newspan = Span.readSpans(ulfstr("::alignment"), graph, sentence)
        spans = newspan :: spans
        annotators = (ulfstr("::annotator") + " " + ulfstr("::date")) :: annotators
    }
    return AMRTriple(sentence, graph, spans.reverse.toArray, annotators.reverse.toArray)
}

object AlignerTool
{
    val usage = """Usage: scala -classpath . edu.cmu.lti.nlp.amr.AlignerTool filename"""
    type OptionMap = Map[Symbol, Any]

    def parseOptions(map : OptionMap, list: List[String]) : OptionMap = {
        def isSwitch(s : String) = (s(0) == '-')
        list match {
            case Nil => map
            case "-v" :: value :: tail =>
                      parseOptions(map ++ Map('verbosity -> value.toInt), tail)
            case string :: opt2 :: tail if isSwitch(opt2) => 
                      parseOptions(map ++ Map('infile -> string), list.tail)
            case string :: Nil =>  parseOptions(map ++ Map('infile -> string), list.tail)
            case option :: tail => println("Error: Unknown option "+option)
                               sys.exit(1)
      }
    }

    def main(args: Array[String]) {

        if (args.length == 0) { println(usage); sys.exit(1) }

        val options = parseOptions(Map(),args.toList)
        if (options.contains('verbosity)) {
            verbosity = options('verbosity).asInstanceOf[Int]
        }
        if (!options.contains('infile)) {
            System.err.println("Error: No AMR file specified")
            sys.exit(1)
        }

        val filename = options('infile).asInstanceOf[String])

        val corpus = LazyArray(splitOnNewline(Source.fromFile(filename).getLines).map(toAMRTriple))

        val mainFrame = new AlignerToolFrame(corpus)
        mainFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
        mainFrame.setSize(640,480)
        mainFrame.setVisible(true)
    }
}

