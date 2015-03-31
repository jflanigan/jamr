Docs - Table of Contents
====

 * Output formats
  * [Alignment Format](./Alignment_Format.md)
  * [Nodes and Edges Format](./Nodes_and_Edges_Format.md)
 * Data
  * [Hand Alignments](./Hand_Alignments.md)
 * Training
  * [Config File](./Config_File.md)
  * [Step by Step Training](./Step_by_Step_Training.md)
 * Evaluation
  * [Alignment Evaluation](./Alignment_Evaluation.md)
  * [Parser Performance](./Parser_Performance.md)
 * Development
  * [**Developer's Guide**](./Developers_Guide.md)

---

Developer's Guide
====

##Git
 - New code should be branched from `dev`.
 - Changes to the core of JAMR in one branch that are potentially useful in other branches should be done in `dev`, or in a separate branch so they can be merged in without bringing in the entire branch.
 - **Do not use `rpull` or `--rebase`.**

##Incremental Compiling

Incremental compiling monitors the source code for changes and compiles them as they happen.  To use incremental compiling, first uncomment this line in `$JAMR_HOME/run`:

    (cd "${MY_DIR}" > /dev/null; ./sbt --error "runMain $klass $*")

and comment out this line:

    #java ${JAVA_OPTS} -cp "${CLASSPATH}" "$klass" "$@"

(This enables running the parser through sbt instead of running the final assembled version that is built with `sbt assembly`)
Then start the incremental compiler in another window:

    $JAMR_HOME/sbt ~compile

