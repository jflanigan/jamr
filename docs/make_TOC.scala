#!/bin/bash
exec scala "$0" "$@"
!#

// Propagates changes in README.md (table of contents) to all other files in docs/
// Usage: cd docs; ./make_TOC.scala

import java.io.File
import java.io.FileWriter
import scala.io.Source.stdin
import scala.io.Source.fromFile

val toc : String = fromFile("README.md").mkString("")

for { file <- new File(".").listFiles
      if file.getName != "README.md"
      if file.getName.matches(".*[.]md") } {
    val lines = fromFile(file).mkString("")
    val contents = lines.split("\n---*\n").drop(1).mkString("\n---\n")
    val writer = new FileWriter(file)
    for (line <- toc.split("\n")) {
        if (line.contains(file.getName)) {
            writer.write(line.replaceAllLiterally("[","[**").replaceAllLiterally("]","**]")+"\n")
        } else {
            writer.write(line+"\n")
        }
    }
    writer.write("\n---\n" + contents)
    writer.close
}

