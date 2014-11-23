#!/bin/bash
set -ueo pipefail

# usage: create a config.sh script, source it, and then run ./TRAIN.sh to train the model

if [ -z "$JAMR_HOME" ]; then
    echo 'Error: please source config script'
    exit 1
fi

pushd "$JAMR_HOME/scripts/preprocessing"
./PREPROCESS.sh
popd

# Train
pushd "$JAMR_HOME/scripts/training"
./cmd.conceptTable.train
echo "Training stage 1"
./cmd.stage1-weights
echo "Training stage 2"
./cmd.stage2-weights

# Evaluate on test set
echo "  *** Test Evaluation (gold concept ID) ***"
./cmd.test.decode.stage2only
"${JAMR_HOME}/scripts/smatch_v1_0/smatch_modified.py" --pr -f "${MODEL_DIR}/test.decode.stage2only" "${TEST_FILE}"
echo "  *** Test Evaluation (all stages) ***"
./cmd.test.decode.allstages
"${JAMR_HOME}/scripts/smatch_v1_0/smatch_modified.py" --pr -f "${MODEL_DIR}/test.decode.allstages" "${TEST_FILE}"

