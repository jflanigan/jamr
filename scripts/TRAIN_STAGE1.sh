#!/bin/bash
set -eo pipefail

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

# Evaluate on test set
echo ""
./cmd.test.decode.stage1only
"${JAMR_HOME}/scripts/smatch_v1_0/smatch_modified.py" --pr -f "${MODEL_DIR}/test.decode.stage1only" "${TEST_FILE}" 2>&1 | tee -a "${MODEL_DIR}/RESULTS.txt"
echo "  ----- Evaluation on Test: Spans -----" | tee -a "${MODEL_DIR}/RESULTS.txt"
tail -n 3 "${MODEL_DIR}/test.decode.stage1only.err" | tee -a "${MODEL_DIR}/RESULTS.txt"
echo ""

