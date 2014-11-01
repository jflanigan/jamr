Docs - Table of Contents
====

 * Output formats
  * [Alignment Format](./Alignment_Format.md)
  * [Nodes and Edges Format](./Nodes_and_Edges_Format.md)
 * Data
  * [**Hand Alignments**](./Hand_Alignments.md)
 * Evaluation
  * [Alignment Evaluation](./Alignment_Evaluation.md)

---

Hand alignments for LDC2012E117
=================

To create the alignments file, move `LDC2013E117.tgz` to `$JAMR_HOME/data` and then do:

    . scripts/config.sh
    scripts/hand_alignments/LDC2013E117/cmd.hand_align.txt

The output will be in `$JAMR_HOME/data/hand_alignments/LDC2013E117/hand_align.txt`

The format is the same as described in [docs/Alignment Format](Alignment_Format.md), except there
are also coreference alignments that are marked with a *.  For example `*15-16|0.0` is a coreference
alignment.  (These coreference alignments are ignored when calculating F1 since JAMR doesn't use
them.)  To evaluate, see [docs/Alignment Evaluation](./Alignment_Evaluation.md).

