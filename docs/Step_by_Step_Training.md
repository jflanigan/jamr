Docs - Table of Contents
====

 * Output formats
  * [Alignment Format](./Alignment_Format.md)
  * [Nodes and Edges Format](./Nodes_and_Edges_Format.md)
 * Data
  * [Hand Alignments](./Hand_Alignments.md)
 * Training
  * [Config File](./Config_File.md)
  * [**Step by Step Training**](./Step_by_Step_Training.md)
 * Evaluation
  * [Alignment Evaluation](./Alignment_Evaluation.md)
  * [Parser Performance](./Parser_Performance.md)
 * Development
  * [Developer's Guide](./Developers_Guide.md)

---

Step by Step Instructions for Training
====

##1. Preprocessing the data

Download and extract `LDC2013E117.tgz` into the directory `data/LDC2013E117_DEFT_Phase_1_AMR_Annotation_R3`.  To make
the train/dev/test split:

    cd scripts/preprocessing
    LDC2013E117/make_splits.sh

Then run `./PREPROCESS.sh` to tokenize, align, and dependency parse the data.


##2. Training

(To skip this step, which takes about 3-6 hours, download and extract model weights
[models.tgz](http://cs.cmu.edu/~jmflanig/models.tgz) into the directory $JAMR_HOME/models.)

    cd scripts/training

Extract concept table:

    ./cmd.conceptTable.train

Concept identification (stage1) training:

    ./cmd.stage1-weights

Relation identification (stage2) training:

    ./cmd.stage2-weights

Search for 'Performance on Dev' in `stage2-weights.err` for early stopping.


##3. Evaluating

Decode test set:

    ./cmd.test.decode.allstages

  or

    ./cmd.test.decode.stage2only

Evaluate the predictions using smatch:

    ${JAMR_HOME}/scripts/smatch_v1_0/smatch_modified.py --pr -f ${MODEL_DIR}/test.decode.stage2only ${TEST_FILE}
    ${JAMR_HOME}/scripts/smatch_v1_0/smatch_modified.py --pr -f ${MODEL_DIR}/test.decode.allstages ${TEST_FILE}

