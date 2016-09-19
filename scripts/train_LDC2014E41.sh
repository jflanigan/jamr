#!/bin/bash
set -ueo pipefail

# Source config script
JAMR_HOME="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." > /dev/null && pwd )"
. "${JAMR_HOME}/scripts/config_Semeval-2016_LDC2014E41.sh"

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
popd
"${JAMR_HOME}/scripts/TRAIN.sh"

