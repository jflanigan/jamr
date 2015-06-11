#!/bin/bash
set -eo pipefail

# Use this script if you want to train and evaluate stage1 only (concept identification)

# usage: create a config.sh script, source it, and then run ./TRAIN_STAGE1.sh to train the model

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

# Evaluate on dev set
echo ""
echo "  ----- Evaluation on Dev: Spans -----" | tee "${MODEL_DIR}/RESULTS.txt"
./cmd.dev.decode.stage1only
tail -n 3 "${MODEL_DIR}/dev.decode.stage1only.err" | tee -a "${MODEL_DIR}/RESULTS.txt"
echo ""

# Evaluate on test set
echo ""
echo "  ----- Evaluation on Test: Spans -----" | tee -a "${MODEL_DIR}/RESULTS.txt"
./cmd.test.decode.stage1only
tail -n 3 "${MODEL_DIR}/test.decode.stage1only.err" | tee -a "${MODEL_DIR}/RESULTS.txt"
echo ""

