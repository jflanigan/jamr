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
  * [**Parser Performance**](./Parser_Performance.md)

---

Parser Performance
=================

This page lists the performance of the parser on various datasets.

## LDC2013E117

Train/dev/test split used in the ACL 2014 paper.  The split is described
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

## LDC2014T12

We follow the split in the `split` directory, and use all the documents.  With the same configuration as the ACL paper
(and aligner updated to handle `have-org-role-91`), we get the following results:

```
  ----- Evaluation on Test: Smatch (all stages) -----
Precision: 0.642
Recall: 0.482
Document F-score: 0.550

  ----- Evaluation on Test: Smatch (gold concept ID) -----
Precision: 0.808
Recall: 0.701
Document F-score: 0.751

  ----- Evaluation on Test: Spans -----
Precision: 0.7032092772384034
Recall: 0.6761750405186385
F1: 0.6894272399775258
```

## LDC2014T12-proxy

We follow the split in the `split` directory, and use only the proxy section.  With the same configuration as the ACL paper
(and aligner updated to handle `have-org-role-91`), we get the following results:

```
  ----- Evaluation on Test: Smatch (all stages) -----
Precision: 0.678
Recall: 0.592
Document F-score: 0.632

  ----- Evaluation on Test: Smatch (gold concept ID) -----
Precision: 0.839
Recall: 0.766
Document F-score: 0.801

  ----- Evaluation on Test: Spans -----
Precision: 0.7141453831041258
Recall: 0.7474809788196587
F1: 0.7304330352657491
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

  ----- Evaluation on Test: Spans -----
Precision: 0.6142484795829714
Recall: 0.6993076162215628
F1: 0.6540240518038852
```

