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
 * Development
  * [Developer's Guide](./Developers_Guide.md)

---

Parser Performance
=================

This page lists the performance of the SemEval 2016 parser and ACL 2014 parser on various datasets.

## SemEval 2016 Parser on LDC2015E86

We follow the split in the `split` directory, and use all the documents.  With the same configuration as the SemEval 2016 paper
(`scripts/config_Semeval-2016_LDC2015E86.sh`), we get the following results:

```
  ----- Evaluation on Test: Smatch (all stages) -----
Precision: 0.697
Recall: 0.645
Document F-score: 0.670

  ----- Evaluation on Test: Smatch (gold concept ID) -----
Precision: 0.833
Recall: 0.740
Document F-score: 0.784

  ----- Evaluation on Test: Spans -----
Precision: 0.7487722619963316
Recall: 0.7913826527421675
F1: 0.7694880214033808
```


## SemEval 2016 Parser on LDC2014T12

We follow the split in the `split` directory, and use all the documents.  With the same configuration as the SemEval 2016 paper
(`scripts/config_Semeval-2016_LDC2014T12.sh`), we get the following results:

```
 ----- Evaluation on Test: Smatch (all stages) -----
Precision: 0.679
Recall: 0.643
Document F-score: 0.660
```


## ACL 2014 Parser on LDC2013E117

Train/dev/test split used in the ACL 2014 paper.  The split is described
[here](../scripts/preprocessing/LDC2013E117/README.md).  With the same configuration as the ACL paper (`scripts/config_ACL2014_LDC2013E117.sh`), and some bugfixes
we get the following results:

```
  ----- Evaluation on Test: Smatch (all stages) -----
Precision: 0.668
Recall: 0.583
Document F-score: 0.623

  ----- Evaluation on Test: Smatch (gold concept ID) -----
Precision: 0.845
Recall: 0.774
Document F-score: 0.808

  ----- Evaluation on Test: Spans -----
Precision: 0.7495288352808142
Recall: 0.7155773469479556
F1: 0.7321597054424117
```

## ACL 2014 Parser on LDC2014E41

We follow the split in the `split` directory, and use all the documents.  With the same configuration as the ACL paper
(`scripts/config_ACL2014_LDC2014E41.sh`) and aligner updated to handle `have-org-role-91`, we get the following results:

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

## ACL 2014 Parser on LDC2014T12

We follow the split in the `split` directory, and use all the documents.  With the same configuration as the ACL paper
(`scripts/config_ACL2014_LDC2014T12.sh`) and aligner updated to handle `have-org-role-91`, we get the following results:

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

## ACL 2014 Parser on LDC2014T12-proxy

We follow the split in the `split` directory, and use only the proxy section.  With the same configuration as the ACL paper
(`scripts/config_ACL2014_LDC2014T12-proxy.sh`) and aligner updated to handle `have-org-role-91`, we get the following results:

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

## ACL 2014 Parser on AMR Bank v1.4 (Little Prince)

Same configuration as ACL paper (`scripts/config_Little_Prince.sh`), with aligner updated to handle `have-org-role-91`.

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

