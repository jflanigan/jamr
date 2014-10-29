Hand alignments for LDC2012E117
=================

To create the alignments file, move `LDC2013E117.tgz` to `$JAMR_HOME/data` and then do:

    . scripts/config.sh
    scripts/hand_alignments/LDC2013E117/cmd.hand_align.txt

The output will be in `$JAMR_HOME/scripts/hand_alignments/LDC2013E117/hand_align.txt`

The format is the same as described in [docs/Alignment_Format.md](docs/Alignment_Format.md), except
there are also coreference alignments that are marked with a *.  For example
`*15-16|0.0` is a coreference alignment.  These coreference alignments are ignored
when calculating F1 since JAMR doesn't use them.

To test the performance of the automatic aligner do:

    cd scripts
    . config.sh
    ./ALIGN.sh < hand_alignments/LDC2013E117/hand_align.txt > align.txt
    ../run EvalSpans < align.txt

The output should be:

    Number of AMR: 200
    Precision = 0.9158829676071055
    Recall = 0.8880445795339412
    F1 = 0.9017489711934157

