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

---

Parser Performance
=================

## LDC2013E117

This is the dataset and train/dev/test split used in the ACL 2014 paper.  The split is listed
[here](../scripts/preprocessing/LDC2013E117/README.md).  With the same configuration as the ACL paper, and some bugfixes
we get the following results:

```
  ----- Evaluation on Test: Smatch (all stages) -----
Precision: 0.667
Recall: 0.583
Document F-score: 0.622

  ----- Evaluation on Test: Smatch (gold concept ID) -----
Precision: 0.845
Recall: 0.775
Document F-score: 0.808
```

## LDC2014E41

We follow the split in the `split` directory, and use all the documents.  With the same configuration as the ACL paper
(and aligner updated to handle `have-org-role-91`), we get the following results:

```
  ----- Evaluation on Test: Smatch (all stages) -----
Precision: 0.646
Recall: 0.531
Document F-score: 0.583

  ----- Evaluation on Test: Smatch (gold concept ID) -----
Precision: 0.809
Recall: 0.700
Document F-score: 0.751
```

## AMR Bank v1.4 (Little Prince)

Same configuration as ACL paper, with aligner updated to handle `have-org-role-91`.

```
  ----- Evaluation on Test: Smatch (all stages) -----
Precision: 0.532
Recall: 0.412
Document F-score: 0.464

  ----- Evaluation on Test: Smatch (gold concept ID) -----
Precision: 0.746
Recall: 0.594
Document F-score: 0.661
```

