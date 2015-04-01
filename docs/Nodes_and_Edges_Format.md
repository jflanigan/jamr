Docs - Table of Contents
====

 * Output formats
  * [Alignment Format](./Alignment_Format.md)
  * [**Nodes and Edges Format**](./Nodes_and_Edges_Format.md)
 * Data
  * [Hand Alignments](./Hand_Alignments.md)
 * Training
  * [Config File](./Config_File.md)
  * [Step by Step Training](./Step_by_Step_Training.md)
 * Evaluation
  * [Alignment Evaluation](./Alignment_Evaluation.md)
  * [Parser Performance](./Parser_Performance.md)
 * Development
  * [Developer's Guide](./Developers_Guide.md)

---

Nodes and Edges Format
===

The parser and aligner both output an additional "nodes and edges" format.  This format lists the nodes and edges of the
AMR graphs in a tab-separated format.

Each sentence is separated by a newline.  The output looks like this:

```
# ::snt The president spoke in November.
# ::tok The president spoke in November .
# ::alignments 0-1|0.1 4-5|0.1.0+0.1 2-3|0 1-2|0.0 ::annotator JAMR dev v0.2 ::date 2014-10-29T20:53:39.435
# ::node        0       speak-01        2-3
# ::node        0.0     president       1-2
# ::node        0.1     date-entity     4-5
# ::node        0.1.0   11      4-5
# ::root        0       speak-01
# ::edge        date-entity     month   11      0.1     0.1.0   
# ::edge        speak-01        ARG0    president       0       0.0
# ::edge        speak-01        time    date-entity     0       0.1
(s / speak-01 
      :ARG0 (p / president) 
      :time (d / date-entity 
            :month 11))
```

Lines beginning with `# ::node` list the nodes.  The format is:

    # ::node TAB uniq_id TAB concept TAB start_alignment-end_alignment

 * `uniq_id` field is a unique id for each node.
 * `concept` field is the node's concept.
 * `start_alignment-end_alignment` field is the span of words in the `# ::tok`
field.  `start_alignment` is inclusive, but `end_alignment` is not.  So 0-1 means the first token in the sentence. If
the node is unaligned, then the `start_alignment-end_alignment` field is blank.

Lines beginning with `# ::root` list the root.  The format is:

    # ::root TAB uniq_id TAB concept

The `uniq_id` is the same as listed in the nodes.  The `concept` field is the concept of the root node.

Lines beginning with `# ::edge` list the edges.  The format is:

    # ::edges TAB source_concept TAB relation TAB dest_concept TAB source_uniq_id TAB dest_uniq_id

The edge points from `source_uniq_id` to `dest_uniq_id` with label `relation`.  `source_concept` and `dest_concept` are
there just for reference.

Nodes are printed first, then the root, and then the edges.

