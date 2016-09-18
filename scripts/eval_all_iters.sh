#!/bin/bash
set -eo pipefail

# usage: create a config.sh script, source it, and then run ./TRAIN.sh to train the model

if [ -z "$JAMR_HOME" ]; then
    echo 'Error: please source config script'
    exit 1
fi

RESULTS="${MODEL_DIR}/RESULTS-alliters.txt"
SMATCH="${JAMR_HOME}/scripts/smatch_v1_0/smatch_modified.py"

echo "" | tee "$RESULTS"

for i in 1 2 3 4 5 6 7 8 9 10; do

echo "Iteration: $i" | tee -a "$RESULTS"

# Evaluate on dev set
echo ""
echo "  ----- Evaluation on Dev: Smatch (all stages) -----" | tee -a "$RESULTS"
#./cmd.dev.decode.allstages
"$SMATCH" --pr -f "${MODEL_DIR}/dev.decode.allstages.iter$i" "${DEV_FILE}" 2>&1 | tee -a "$RESULTS"
echo "" | tee -a "$RESULTS"
#echo "  ----- Evaluation on Dev: Smatch (gold concept ID) -----" | tee -a "$RESULTS"
#./cmd.dev.decode.stage2only
#"$SMATCH" --pr -f "${MODEL_DIR}/dev.decode.stage2only.iter$i" "${DEV_FILE}" 2>&1 | tee -a "$RESULTS"
#echo "" | tee -a "$RESULTS"
#echo "  ----- Evaluation on Dev: Spans -----" | tee -a "$RESULTS"
#tail -n 3 "${MODEL_DIR}/dev.decode.allstages.iter$i.err" | tee -a "$RESULTS"
#echo "" | tee -a "$RESULTS"

# Evaluate on test set
echo "" | tee -a "$RESULTS"
echo "  ----- Evaluation on Test: Smatch (all stages) -----" | tee -a "$RESULTS"
#./cmd.test.decode.allstages
"$SMATCH" --pr -f "${MODEL_DIR}/test.decode.allstages.iter$i" "${TEST_FILE}" 2>&1 | tee -a "$RESULTS"
echo "" | tee -a "$RESULTS"
#echo "  ----- Evaluation on Test: Smatch (gold concept ID) -----" | tee -a "$RESULTS"
#./cmd.test.decode.stage2only
#"$SMATCH" --pr -f "${MODEL_DIR}/test.decode.stage2only.iter$i" "${TEST_FILE}" 2>&1 | tee -a "$RESULTS"
#echo "" | tee -a "$RESULTS"
#echo "  ----- Evaluation on Test: Spans -----" | tee -a "$RESULTS"
#tail -n 3 "${MODEL_DIR}/test.decode.allstages.iter$i.err" | tee -a "$RESULTS"
echo ""

done

