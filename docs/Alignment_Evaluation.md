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
  * [**Alignment Evaluation**](./Alignment_Evaluation.md)
  * [Parser Performance](./Parser_Performance.md)
 * Development
  * [Developer's Guide](./Developers_Guide.md)

---

Alignment Evaluation
=================

Alignments can be evaluated using [EvalSpans](../src/EvalSpans.scala).  EvalSpans expects the gold alignments to be
marked with `::gold`.  For example:

```
# ::tok Saudi Arabia ( SA )
# ::alignments 0-2|0+0.0+0.0.0+0.0.1 ::gold
# ::alignments 0-2|0+0.0+0.0.0+0.0.1
(c / country
  :name (n / name
          :op1 "Saudi"
          :op2 "Arabia"))
```

To test the performance of the automatic aligner, first create the [hand alignments file](./Hand_Alignments.md) and then
do:

    . scripts/config.sh
    scripts/ALIGN.sh < data/hand_alignments/LDC2013E117/hand_align.txt > align.txt
    ./run EvalSpans < align.txt

The output should be:

    Number of AMR: 200
    Precision = 0.9158829676071055
    Recall = 0.8880445795339412
    F1 = 0.9017489711934157

