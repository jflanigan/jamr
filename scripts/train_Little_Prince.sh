#!/bin/bash
set -ueo pipefail

# Source config script
JAMR_HOME="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." > /dev/null && pwd )"
. "${JAMR_HOME}/scripts/config_Little_Prince.sh"

mkdir -p "$JAMR_HOME/data/AMR-Bank-v1.4"
pushd "$JAMR_HOME/data/AMR-Bank-v1.4"
wget http://amr.isi.edu/download/amr-bank-struct-v1.4-training.txt
wget http://amr.isi.edu/download/amr-bank-struct-v1.4-dev.txt
wget http://amr.isi.edu/download/amr-bank-struct-v1.4-test.txt
popd

pushd "$JAMR_HOME/scripts/preprocessing"
LDC2013E117/make_splits.sh
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

