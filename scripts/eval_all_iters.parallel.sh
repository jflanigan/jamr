#!/bin/bash
set -eo pipefail

# usage: create a config.sh script, source it, and then run ./TRAIN.sh to train the model

if [ -z "$JAMR_HOME" ]; then
    echo 'Error: please source config script'
    exit 1
fi

SMATCH="${JAMR_HOME}/scripts/smatch_v1_0/smatch_modified.py"

for i in 1 2 3 4 5 6 7 8 9 10; do

"$SMATCH" --pr -f "${MODEL_DIR}/dev.decode.allstages.iter$i" "${DEV_FILE}" &> "${MODEL_DIR}/dev.decode.allstages.iter$i.eval" &
"$SMATCH" --pr -f "${MODEL_DIR}/test.decode.allstages.iter$i" "${TEST_FILE}" &> "${MODEL_DIR}/test.decode.allstages.iter$i.eval" &

done

wait
