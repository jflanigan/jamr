#!/bin/bash
set -ueo pipefail

# Source config script
JAMR_HOME="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." > /dev/null && pwd )"
. "${JAMR_HOME}/scripts/config_ACL2014_LDC2014E41.sh"

# Extract and preprocess data
if [ ! -f "$JAMR_HOME/data/LDC2014E41_DEFT_Phase_1_AMR_Annotation_R4.tgz" ]; then
    echo 'Error: Please copy LDC2014E41_DEFT_Phase_1_AMR_Annotation_R4.tgz to the directory $JAMR_HOME/data before running this script.'
    exit 1
fi
mkdir -p "$JAMR_HOME/data"
pushd "$JAMR_HOME/data"
tar -xzf LDC2014E41_DEFT_Phase_1_AMR_Annotation_R4.tgz
popd

pushd "$JAMR_HOME/scripts/preprocessing"
LDC2014E41/make_splits.sh
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

