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

class AlignerToolFrame((Int) => AMRTriple) extends JFrame("AMR Aligner Tool v0.1a") {
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

case class AMRTriple(sentence: Array[String], graph: Graph, spans: Spans)
def splitOnNewline(iterator: Iterator[String]) : Iterator[String] = {
    val it = Iterator[String].concat(Iterator("\n", iterator)   // pad a first newline which will get dropped
    for {
        x <- it
        p = it.takewhile(_ != "\n").mkString
    } yield p
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

    val buffer = ArrayBuffer[AMRTriple]()

    def getAMR(index: Int) : AMRTriple = {
        if (index < buffer.size) {
            buffer(index)
        } else {
            while (index < buffer.size) {
                
                buffer += 
            }
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

        val mainFrame = new AlignerToolFrame(getAMR)
        mainFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
        mainFrame.setSize(640,480)
        mainFrame.setVisible(true)
    }
}

